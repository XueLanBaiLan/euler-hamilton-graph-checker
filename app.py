#欧拉图 / 哈密顿图判断器
#开发 : 雪兰白兰(Xuelanbailan)
#开源的 , 扣 1 送学时

import tkinter as tk
from tkinter import messagebox, ttk
from math import hypot
from collections import defaultdict

class GraphApp:
    def __init__(self, root):
        self.root = root
        self.root.title("欧拉图 / 哈密顿图判断器")
        self.root.geometry("1000x700")

        #基础参数
        self.rows = tk.IntVar(value=4)
        self.cols = tk.IntVar(value=4)
        self.node_radius = 12
        self.padding = 40
        self.spacing = 80
        self.petersen_n = tk.IntVar(value=5)  #彼得森外圈顶点数（>=5）

        #模式：有向/无向，添加/删除节点模式（互斥）
        self.directed = tk.BooleanVar(value=True)
        self.add_node_mode = tk.BooleanVar(value=False)
        self.delete_node_mode = tk.BooleanVar(value=False)
        self.show_edge_labels = tk.BooleanVar(value=False)

        #图数据：邻接表存出边 {to: multiplicity 1~3}
        self.nodes = []  #节点坐标 (x, y)
        self.adj = {}
        self.edge_labels = {}  #key -> name
        self.edge_seq = 1
        self.selected = None
        
        #动画相关
        self.animation_path = []  #当前动画路径 [(u, v, k), ...] 表示边(u,v)的第k条平行边
        self.animation_vertices = []  #顶点路径（用于欧拉图）
        self.animation_type = None  #'euler' 或 'hamilton'
        self.animation_index = 0
        self.animation_phase = 'vertex'  #'vertex' 或 'edge'
        self.animation_running = False
        self.animation_speed = 500  #毫秒
        self.animation_after_id = None
        self.highlighted_edges = set()  #当前高亮的边
        self.highlighted_vertex = None  #当前高亮的顶点
        self.vertex_scale = 1.0  #顶点闪烁缩放
        self.vertex_scale_direction = 'up'  #'up' 或 'down'，用于跟踪放大/缩小方向

        #构建界面
        self._build_controls()
        self._build_canvas()
        self._rebuild_grid()

    def _build_controls(self):
        """构建控制栏（带输入验证）"""
        frm = tk.Frame(self.root)
        frm.pack(fill=tk.X, padx=10, pady=6)

        #行列输入（仅允许正整数）
        tk.Label(frm, text="行:").pack(side=tk.LEFT)
        row_entry = tk.Entry(frm, textvariable=self.rows, width=4)
        row_entry.pack(side=tk.LEFT, padx=4)
        row_entry.bind("<FocusOut>", lambda e: self._validate_int(self.rows, 2))

        tk.Label(frm, text="列:").pack(side=tk.LEFT)
        col_entry = tk.Entry(frm, textvariable=self.cols, width=4)
        col_entry.pack(side=tk.LEFT, padx=4)
        col_entry.bind("<FocusOut>", lambda e: self._validate_int(self.cols, 2))

        #第一行按钮
        row1 = tk.Frame(frm)
        row1.pack(fill=tk.X, pady=2)
        tk.Button(row1, text="重建格点", command=self._rebuild_grid).pack(side=tk.LEFT, padx=4)
        tk.Button(row1, text="清空边", command=self._clear_edges).pack(side=tk.LEFT, padx=4)
        tk.Button(row1, text="检查欧拉/哈密顿", command=self._check_graph).pack(side=tk.LEFT, padx=4)
        tk.Checkbutton(row1, text="有向模式", variable=self.directed, command=self._toggle_mode).pack(side=tk.LEFT, padx=4)
        tk.Checkbutton(row1, text="添加节点模式", variable=self.add_node_mode, command=self._toggle_add_mode).pack(side=tk.LEFT, padx=4)
        tk.Checkbutton(row1, text="删除节点模式", variable=self.delete_node_mode, command=self._toggle_delete_mode).pack(side=tk.LEFT, padx=4)
        tk.Checkbutton(row1, text="显示边名称", variable=self.show_edge_labels, command=self._draw_all).pack(side=tk.LEFT, padx=4)

        #第二行：预设图与参数
        row2 = tk.Frame(frm)
        row2.pack(fill=tk.X, pady=2)
        tk.Button(row2, text="经典彼得森图", command=self._build_classic_petersen).pack(side=tk.LEFT, padx=4)
        tk.Button(row2, text="广义彼得森图", command=self._build_petersen).pack(side=tk.LEFT, padx=4)
        tk.Label(row2, text="外圈顶点数:").pack(side=tk.LEFT, padx=(10, 2))
        pet_entry = tk.Entry(row2, textvariable=self.petersen_n, width=4)
        pet_entry.pack(side=tk.LEFT)
        pet_entry.bind("<FocusOut>", lambda e: self._validate_int(self.petersen_n, 5))

        #状态提示
        self.status_var = tk.StringVar(value="提示：点击两个点可添加/删除边（最多3条平行边）")
        status_label = tk.Label(self.root, textvariable=self.status_var, fg="#006400", anchor="w")
        status_label.pack(fill=tk.X, padx=10, pady=4)

    def _validate_int(self, var, min_val):
        """验证整数输入，确保≥min_val"""
        try:
            val = int(var.get())
            var.set(max(val, min_val))
        except ValueError:
            var.set(min_val)
            messagebox.showwarning("输入错误", "请输入正整数！")

    def _build_canvas(self):
        """构建带滚动条的画布"""
        canvas_frame = ttk.Frame(self.root)
        canvas_frame.pack(fill=tk.BOTH, expand=True)
        #滚动条
        x_scroll = ttk.Scrollbar(canvas_frame, orient=tk.HORIZONTAL)
        y_scroll = ttk.Scrollbar(canvas_frame, orient=tk.VERTICAL)
        x_scroll.pack(side=tk.BOTTOM, fill=tk.X)
        y_scroll.pack(side=tk.RIGHT, fill=tk.Y)

        #画布（绑定滚动条）
        self.canvas = tk.Canvas(
            canvas_frame,
            bg="#fafafa",
            xscrollcommand=x_scroll.set,
            yscrollcommand=y_scroll.set
        )
        self.canvas.pack(fill=tk.BOTH, expand=True)
        x_scroll.config(command=self.canvas.xview)
        y_scroll.config(command=self.canvas.yview)

        #绑定事件
        self.canvas.bind("<Button-1>", self._on_click)
        self.canvas.bind("<Configure>", lambda e: self.canvas.configure(scrollregion=self.canvas.bbox("all")))

    def _rebuild_grid(self):
        """重建节点网格（自适应画布）"""
        r, c = self.rows.get(), self.cols.get()
        self.nodes = []
        self.adj = {i: {} for i in range(r * c)}
        self.selected = None

        #计算节点间距（自适应画布大小）
        canvas_w = max(800, self.canvas.winfo_width())
        canvas_h = max(600, self.canvas.winfo_height())
        self.spacing = max(50, int(min(
            (canvas_w - 2 * self.padding) / max(1, c - 1),
            (canvas_h - 2 * self.padding) / max(1, r - 1)
        )))

        #生成节点坐标
        for i in range(r):
            for j in range(c):
                x = self.padding + j * self.spacing
                y = self.padding + i * self.spacing
                self.nodes.append((x, y))

        self._draw_all()
        self.status_var.set(f"已创建 {r}x{c} 格点（共{r*c}个节点），点击两个点可连线")

    def _build_classic_petersen(self):
        """构建经典彼得森图（固定10个顶点：外圈五边形+内圈五角星）"""
        import math
        
        #基于当前画布尺寸居中
        canvas_w = max(800, self.canvas.winfo_width())
        canvas_h = max(600, self.canvas.winfo_height())
        cx, cy = canvas_w / 2, canvas_h / 2
        outer_r = min(canvas_w, canvas_h) * 0.42
        inner_r = outer_r * 0.55
        
        self.nodes = []
        self.adj = {i: {} for i in range(10)}
        self.selected = None
        
        #外层5个顶点（五边形）
        for k in range(5):
            ang = math.radians(90 - k * 72)  #72度 = 360/5
            x = cx + outer_r * math.cos(ang)
            y = cy - outer_r * math.sin(ang)
            self.nodes.append((x, y))
        
        #内层5个顶点（五角星）
        for k in range(5):
            ang = math.radians(90 - k * 72 + 36)  #偏移36度（180/5）
            x = cx + inner_r * math.cos(ang)
            y = cy - inner_r * math.sin(ang)
            self.nodes.append((x, y))
        
        #圈五边形边：0-1, 1-2, 2-3, 3-4, 4-0
        for k in range(5):
            self._add_edge_preset(k, (k + 1) % 5)
        
        #内圈五角星边：步长2连接（5-7, 6-8, 7-9, 8-5, 9-6）
        for k in range(5):
            inner_a = 5 + k
            inner_b = 5 + ((k + 2) % 5)
            self._add_edge_preset(inner_a, inner_b)
        
        #辐射连线：外圈顶点k连接到内圈顶点k
        for k in range(5):
            self._add_edge_preset(k, 5 + k)
        
        self._draw_all()
        self.status_var.set("已生成经典彼得森图（10节点，15边）。可继续添加/删除边。")

    def _build_petersen(self):
        """构建广义彼得森图 G(n,2)：外圈 n 边形，内圈五角星式连边，n>=5"""
        n = max(5, int(self.petersen_n.get()))
        self.petersen_n.set(n)

        #基于当前画布尺寸居中
        canvas_w = max(800, self.canvas.winfo_width())
        canvas_h = max(600, self.canvas.winfo_height())
        cx, cy = canvas_w / 2, canvas_h / 2
        outer_r = min(canvas_w, canvas_h) * 0.42
        inner_r = outer_r * 0.55

        import math
        self.nodes = []
        self.adj = {i: {} for i in range(2 * n)}
        self.selected = None

        #外层 n 点
        for k in range(n):
            ang = math.radians(90 - k * 360 / n)
            x = cx + outer_r * math.cos(ang)
            y = cy - outer_r * math.sin(ang)
            self.nodes.append((x, y))
        #内层 n 点
        for k in range(n):
            ang = math.radians(90 - k * 360 / n + 180 / n)
            x = cx + inner_r * math.cos(ang)
            y = cy - inner_r * math.sin(ang)
            self.nodes.append((x, y))

        #外圈 n 边形
        for k in range(n):
            a = k
            b = (k + 1) % n
            self._add_edge_preset(a, b)
        #内圈五角星式：步长 2（若 n=5 即经典彼得森）
        for k in range(n):
            a = n + k
            b = n + ((k + 2) % n)
            self._add_edge_preset(a, b)
        #辐射连线：外 k 到内 k
        for k in range(n):
            self._add_edge_preset(k, n + k)

        self._draw_all()
        self.status_var.set(f"已生成广义彼得森图 G({n},2)（{2*n} 节点）。可继续添加/删除边。")

    def _clear_edges(self):
        """清空所有边"""
        for k in self.adj:
            self.adj[k].clear()
        self.edge_labels.clear()
        self.edge_seq = 1
        self.selected = None
        self._draw_all()
        self.status_var.set("已清空所有边")

    def _toggle_mode(self):
        """切换有向/无向模式，并同步邻接表"""
        if not self.directed.get():
            #切换到无向：确保邻接对称
            for a in list(self.adj.keys()):
                for b, cnt in list(self.adj[a].items()):
                    if cnt <= 0:
                        continue
                    self.adj.setdefault(b, {})
                    self.adj[b][a] = max(self.adj[b].get(a, 0), cnt)
        self._draw_all()
        mode = "有向" if self.directed.get() else "无向"
        self.status_var.set(f"已切换为{mode}模式")

    def _toggle_add_mode(self):
        """添加节点模式启用时，自动关闭删除模式"""
        if self.add_node_mode.get():
            self.delete_node_mode.set(False)
            self.status_var.set("添加节点模式：点击空白处添加节点")
        else:
            self.status_var.set("提示：点击两个点可添加/删除边（最多3条平行边）")

    def _toggle_delete_mode(self):
        """删除节点模式启用时，自动关闭添加模式"""
        if self.delete_node_mode.get():
            self.add_node_mode.set(False)
            self.status_var.set("删除节点模式：点击节点将其删除")
        else:
            self.status_var.set("提示：点击两个点可添加/删除边（最多3条平行边）")

    def _edge_label_key(self, a, b, k):
        """生成边标签键，k 为第 k 条平行边（1-based）"""
        if not self.directed.get():
            a, b = sorted((a, b))
        return (a, b, k)

    def _prune_edge_labels(self, a, b, keep_cnt):
        """删除多余的平行边标签"""
        to_del = []
        for key in self.edge_labels:
            u, v, k = key
            if not self.directed.get():
                if tuple(sorted((a, b))) != tuple(sorted((u, v))):
                    continue
            else:
                if (u, v) != (a, b):
                    continue
            if k > keep_cnt:
                to_del.append(key)
        for key in to_del:
            self.edge_labels.pop(key, None)

    def _reset_labels_if_empty(self):
        """若当前无边，重置标签计数到 e1"""
        has_edge = any(
            cnt > 0
            for _, nbrs in self.adj.items()
            for cnt in nbrs.values()
        )
        if not has_edge:
            self.edge_labels.clear()
            self.edge_seq = 1

    #---------- 欧拉路径构造 ----------
    def _find_euler_undirected_path(self, active, prefer_circuit, degrees):
        #仅在已知存在欧拉通路/回路时调用
        adj = defaultdict(list)
        edge_labels = {}
        edge_id = 0
        #构建无向多重图的边列表（每条边仅加入一次）
        for u in active:
            for v, cnt in self.adj[u].items():
                if cnt <= 0 or v not in active:
                    continue
                if v < u:
                    continue  #避免重复
                for k in range(cnt):
                    key = self._edge_label_key(u, v, k + 1)
                    label = self.edge_labels.get(key, "")
                    edge_labels[edge_id] = label
                    adj[u].append((v, edge_id))
                    adj[v].append((u, edge_id))
                    edge_id += 1
        if edge_id == 0:
            return [], []

        #选起点：若有 2 个奇度点则用其一，否则任意
        odd = [v for v in active if degrees[v] % 2 == 1]
        if prefer_circuit or len(odd) == 0:
            start = active[0]
        else:
            start = odd[0]

        used = set()
        stack = [start]
        edge_stack = []
        circuit_v = []
        circuit_e = []

        while stack:
            v = stack[-1]
            while adj[v] and adj[v][-1][1] in used:
                adj[v].pop()
            if adj[v]:
                to, eid = adj[v].pop()
                if eid in used:
                    continue
                used.add(eid)
                stack.append(to)
                edge_stack.append(eid)
            else:
                circuit_v.append(stack.pop())
                if edge_stack:
                    circuit_e.append(edge_stack.pop())

        path_vertices = circuit_v[::-1]
        path_edges = [edge_labels[eid] for eid in circuit_e[::-1]]
        return path_vertices, path_edges

    def _find_euler_directed_path(self, active, indeg, outdeg, prefer_circuit):
        #构建有向多重图
        adj = defaultdict(list)
        edge_labels = {}
        edge_id = 0
        for u in active:
            for v, cnt in self.adj[u].items():
                if cnt <= 0 or v not in active:
                    continue
                for k in range(cnt):
                    key = self._edge_label_key(u, v, k + 1)
                    label = self.edge_labels.get(key, "")
                    edge_labels[edge_id] = label
                    adj[u].append((v, edge_id))
                    edge_id += 1
        if edge_id == 0:
            return [], []

        diff = {v: outdeg[v] - indeg[v] for v in active}
        pos = [v for v, d in diff.items() if d == 1]
        neg = [v for v, d in diff.items() if d == -1]
        #起点：回路用任意；通路用差值 +1 的点，否则任意
        if prefer_circuit or (len(pos) == 0 and len(neg) == 0):
            start = active[0]
        elif len(pos) == 1:
            start = pos[0]
        else:
            start = active[0]

        used = set()
        stack = [start]
        edge_stack = []
        circuit_v = []
        circuit_e = []

        while stack:
            v = stack[-1]
            while adj[v] and adj[v][-1][1] in used:
                adj[v].pop()
            if adj[v]:
                to, eid = adj[v].pop()
                if eid in used:
                    continue
                used.add(eid)
                stack.append(to)
                edge_stack.append(eid)
            else:
                circuit_v.append(stack.pop())
                if edge_stack:
                    circuit_e.append(edge_stack.pop())

        path_vertices = circuit_v[::-1]
        path_edges = [edge_labels[eid] for eid in circuit_e[::-1]]
        return path_vertices, path_edges

    def _add_edge_preset(self, a, b, cnt=1):
        """用于预设构图：根据当前模式添加边"""
        self.adj[a][b] = cnt
        if not self.directed.get() and a != b:
            self.adj[b][a] = cnt
        #预设边标签
        for k in range(1, cnt + 1):
            key = self._edge_label_key(a, b, k)
            if key not in self.edge_labels:
                self.edge_labels[key] = f"e{self.edge_seq}"
                self.edge_seq += 1

    def _delete_node(self, idx):
        """删除节点及相关边，不重排索引"""
        if idx >= len(self.nodes) or self.nodes[idx] is None:
            return
        #删除指向该节点的边
        for u in list(self.adj.keys()):
            self.adj[u].pop(idx, None)
        #删除该节点的出边
        self.adj.pop(idx, None)
        #标记节点无效
        self.nodes[idx] = (None, None)
        #删除相关边标签
        to_del = [k for k in self.edge_labels if idx in k[:2]]
        for k in to_del:
            self.edge_labels.pop(k, None)
        self._reset_labels_if_empty()

    def _on_click(self, event):
        """处理画布点击：选中节点/切换边/添加节点"""
        #转换滚动后的坐标
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        
        idx = self._nearest_node(x, y)

        #删除节点模式
        if idx is not None and self.delete_node_mode.get():
            self._delete_node(idx)
            self.selected = None
            self._draw_all()
            self.status_var.set(f"已删除节点 {idx}")
            return

        #添加节点模式
        if idx is None and self.add_node_mode.get():
            new_idx = len(self.nodes)
            self.nodes.append((x, y))
            self.adj[new_idx] = {}
            self.selected = None
            self._draw_all()
            self.status_var.set(f"已添加节点 {new_idx}")
            return

        if idx is None:
            return
        
        if self.selected is None:
            self.selected = idx
            self._draw_all()
            return
        
        if self.selected == idx:
            self._toggle_edge(self.selected, idx)
            self.selected = None
        else:
            self._toggle_edge(self.selected, idx)
            self.selected = None
        self._draw_all()

    def _nearest_node(self, x, y):
        """找点击位置最近的节点"""
        for idx, (nx, ny) in enumerate(self.nodes):
            if nx is None or ny is None:
                continue
            if hypot(nx - x, ny - y) <= self.node_radius * 1.5:
                return idx
        return None

    def _toggle_edge(self, a, b):
        """切换边数（有向/无向，0-3循环）"""
        cur = self.adj[a].get(b, 0)
        nxt = (cur + 1) if cur < 3 else 0
        if nxt == 0:
            self.adj[a].pop(b, None)
            if not self.directed.get() and a != b:
                self.adj[b].pop(a, None)
            #移除超出计数的标签
            self._prune_edge_labels(a, b, 0)
            self._reset_labels_if_empty()
        else:
            self.adj[a][b] = nxt
            if not self.directed.get() and a != b:
                self.adj[b][a] = nxt
            #新增标签
            key = self._edge_label_key(a, b, nxt)
            if key not in self.edge_labels:
                self.edge_labels[key] = f"e{self.edge_seq}"
                self.edge_seq += 1
            #修剪多余标签
            self._prune_edge_labels(a, b, nxt)

    def _draw_all(self):
        """绘制所有节点和边"""
        self.canvas.delete("all")
        
        #绘制边（平行边/自环，依据有向/无向绘制箭头）
        for i, nbrs in self.adj.items():
            if i >= len(self.nodes) or self.nodes[i] is None:
                continue
            for j, cnt in nbrs.items():
                if cnt <= 0:
                    continue
                if j >= len(self.nodes) or self.nodes[j] is None:
                    continue
                if not self.directed.get() and j < i:
                    #无向模式只画一次
                    continue
                self._draw_edge(i, j, cnt)

        #绘制节点（选中节点高亮，动画顶点闪烁）
        for idx, (x, y) in enumerate(self.nodes):
            if x is None or y is None:
                continue
            #判断颜色和大小
            if idx == self.selected:
                color = "#ff9800"
                radius = self.node_radius
            elif idx == self.highlighted_vertex:
                #动画闪烁的顶点：红色，带缩放
                color = "#ff0000"
                radius = self.node_radius * self.vertex_scale
            else:
                color = "#1976d2"
                radius = self.node_radius
            
            self.canvas.create_oval(
                x - radius, y - radius,
                x + radius, y + radius,
                fill=color, outline="#0d47a1", width=2
            )
            self.canvas.create_text(x, y, text=str(idx), fill="white", font=("Arial", 10, "bold"))

        #更新滚动区域
        self.canvas.configure(scrollregion=self.canvas.bbox("all"))


    def _draw_self_loop(self, i, cnt):
        """绘制自环（分层避免重叠）"""
        x, y = self.nodes[i]
        base_r = self.node_radius + 8
        
        arrow_flag = tk.LAST if self.directed.get() else None
        #自环绘制完整椭圆，再在椭圆上加箭头，避免残缺
        angles = [90, 45, 135][:cnt]
        for k, angle in enumerate(angles):
            r = base_r + k * 10
            #检查是否高亮
            edge_key = (i, i, k + 1)
            is_highlighted = edge_key in self.highlighted_edges
            edge_color = "#ff0000" if is_highlighted else "#444"
            edge_width = 4 if is_highlighted else 2
            self.canvas.create_oval(
                x - r, y - r - 10,
                x + r, y + r - 10,
                outline=edge_color,
                width=edge_width,
            )
            #标签
            if self.show_edge_labels.get():
                key = self._edge_label_key(i, i, k + 1)
                label = self.edge_labels.get(key, "")
                if label:
                    self.canvas.create_text(x, y - r - 16, text=label, fill="#000", font=("Arial", 9, "bold"))
            if arrow_flag:
                import math
                rad = math.radians(angle)
                #选定一点作为箭头位置
                ax = x + r * math.cos(rad)
                ay = y - 10 + r * math.sin(rad)
                #切线方向
                tangent_angle = rad + math.pi / 2
                ex = ax + 14 * math.cos(tangent_angle)
                ey = ay + 14 * math.sin(tangent_angle)
                self.canvas.create_line(
                    ax, ay, ex, ey,
                    fill="#444",
                    width=2,
                    arrow=arrow_flag,
                    arrowshape=(14, 18, 10),
                    capstyle=tk.ROUND,
                )

    #---------------------- 图论分析核心 ----------------------
    def _active_vertices_directed(self):
        """有向：返回活跃节点及入度/出度/总度"""
        indeg = {v: 0 for v in self.adj}
        outdeg = {v: 0 for v in self.adj}
        for v, nbrs in self.adj.items():
            for nb, cnt in nbrs.items():
                if cnt <= 0:
                    continue
                outdeg[v] += cnt
                indeg[nb] += cnt
        degree_sum = {v: indeg[v] + outdeg[v] for v in self.adj}
        active = [v for v, d in degree_sum.items() if d > 0]
        if not active:
            active = list(self.adj.keys())
        return active, indeg, outdeg, degree_sum

    def _active_vertices_undirected(self):
        """无向：返回活跃节点及度数（自环计2，普通边计1）"""
        degrees = {}
        for v, nbrs in self.adj.items():
            deg = 0
            for nb, cnt in nbrs.items():
                if cnt <= 0:
                    continue
                if nb == v:
                    deg += cnt * 2
                else:
                    deg += cnt
            degrees[v] = deg
        active = [v for v, d in degrees.items() if d > 0]
        if not active:
            active = list(self.adj.keys())
        return active, degrees

    def _components(self, vertices):
        """DFS找连通分量（忽略方向，只看是否存在边相连）"""
        vertex_set = set(vertices)
        #构建无向邻接（只在活跃节点内）
        undirected = {v: set() for v in vertex_set}
        for v in vertex_set:
            for nb, cnt in self.adj[v].items():
                if cnt > 0 and nb in vertex_set:
                    undirected[v].add(nb)
                    undirected[nb].add(v)
        seen = set()
        comps = []
        for v in vertex_set:
            if v in seen:
                continue
            stack = [v]
            seen.add(v)
            comp = [v]
            while stack:
                cur = stack.pop()
                for nb in undirected[cur]:
                    if nb not in seen:
                        seen.add(nb)
                        stack.append(nb)
                        comp.append(nb)
            comps.append(comp)
        return comps

    def _analyze_euler(self):
        """分析欧拉特性，支持有向/无向"""
        if self.directed.get():
            return self._analyze_euler_directed()
        return self._analyze_euler_undirected()
    
    def _analyze_euler_with_path(self):
        """分析欧拉特性并返回路径信息"""
        if self.directed.get():
            return self._analyze_euler_directed_with_path()
        return self._analyze_euler_undirected_with_path()

    def _analyze_euler_directed(self):
        active, indeg, outdeg, degree_sum = self._active_vertices_directed()
        if all(d == 0 for d in degree_sum.values()):
            return "欧拉（有向）：否（无任何边，无法构成通路/回路）"
        
        comps = self._components(active)
        k = len(comps)

        diff = {v: outdeg[v] - indeg[v] for v in active}
        pos = [v for v, d in diff.items() if d > 0]
        neg = [v for v, d in diff.items() if d < 0]
        total_pos = sum(diff[v] for v in pos)

        has_circuit = k == 1 and total_pos == 0
        has_trail = False
        if k == 1:
            if total_pos == 0:
                has_trail = True
            elif total_pos == 1 and len(pos) == 1 and len(neg) == 1:
                has_trail = True

        #估计最少增边数（更保守的上界）：连通与平衡同时考虑
        edges_for_connect = max(0, k - 1)
        #为回路：需弱连通且全部 in=out。若用连接边同时用于平衡，需求不超过二者较大者。
        need_circuit = max(edges_for_connect, total_pos)
        #为通路：允许一个 +1 起点和一个 -1 终点
        need_trail = max(edges_for_connect, max(0, total_pos - 1))

        suggest = self._suggest_edge_directed(active, comps, diff, pos, neg)

        euler_path_txt = ""
        if has_circuit or has_trail:
            path_v, edge_seq = self._find_euler_directed_path(active, indeg, outdeg, prefer_circuit=has_circuit)
            if path_v:
                path_str = " → ".join(map(str, path_v))
                if self.show_edge_labels.get() and edge_seq:
                    edge_str = " , ".join(edge_seq)
                    euler_path_txt = f"\n  → 示例路径：{path_str}\n  → 经过边：{edge_str}"
                else:
                    euler_path_txt = f"\n  → 示例路径：{path_str}"

        if has_circuit:
            return (
                "欧拉（有向）：是（存在有向欧拉回路）\n"
                f"  → 连通分量={k}（按无向视角），全部顶点 in=out"
                + euler_path_txt
            )
        if has_trail:
            return (
                "欧拉（有向）：半欧拉（有通路，无回路）\n"
                f"  → 连通分量={k}，出入度差满足通路条件"
                + euler_path_txt
            )
        return (
            "欧拉（有向）：否\n"
            f"  → 连通分量={k}，出入度不平衡顶点 {len(pos)+len(neg)} 个\n"
            f"  → 缺 {need_trail} 条边可成半欧拉（通路），缺 {need_circuit} 条边可成欧拉（回路）\n"
            f"  → 建议先加：{suggest}"
        )

    def _analyze_euler_undirected(self):
        active, degrees = self._active_vertices_undirected()
        if all(d == 0 for d in degrees.values()):
            return "欧拉（无向）：否（无任何边，无法构成通路/回路）"

        comps = self._components(active)
        k = len(comps)
        odd_vertices = [v for v in active if degrees[v] % 2 == 1]
        o = len(odd_vertices)

        has_circuit = k == 1 and o == 0
        has_trail = k == 1 and o in (0, 2)

        edges_for_connect = max(0, k - 1)
        paired_by_connect = min(edges_for_connect, o // 2)
        remaining_odd = o - paired_by_connect * 2

        need_trail = edges_for_connect + max(0, (remaining_odd - 2) // 2)
        need_circuit = edges_for_connect + (remaining_odd // 2)

        suggest = self._suggest_edge_undirected(comps, odd_vertices)

        euler_path_txt = ""
        if has_circuit or has_trail:
            path_v, edge_seq = self._find_euler_undirected_path(active, has_circuit, degrees)
            if path_v:
                path_str = " → ".join(map(str, path_v))
                if self.show_edge_labels.get() and edge_seq:
                    edge_str = " , ".join(edge_seq)
                    euler_path_txt = f"\n  → 示例路径：{path_str}\n  → 经过边：{edge_str}"
                else:
                    euler_path_txt = f"\n  → 示例路径：{path_str}"

        if has_circuit:
            return f"欧拉（无向）：是（存在欧拉回路）\n  → 连通分量={k}，奇度顶点={o}" + euler_path_txt
        if has_trail:
            return (
                "欧拉（无向）：半欧拉（有通路，无回路）\n"
                f"  → 连通分量={k}，奇度顶点={o}"
                + euler_path_txt
            )
        return (
            "欧拉（无向）：否\n"
            f"  → 连通分量={k}，奇度顶点={o}\n"
            f"  → 缺 {need_trail} 条边可成半欧拉（通路），缺 {need_circuit} 条边可成欧拉（回路）\n"
            f"  → 建议先加：{suggest}"
        )

    def _analyze_hamiltonian(self):
        """分析哈密顿特性，支持有向/无向"""
        if self.directed.get():
            return self._analyze_hamiltonian_directed()
        return self._analyze_hamiltonian_undirected()

    #---------- 辅助建议：欧拉加边 ----------
    def _suggest_edge_undirected(self, comps, odd_vertices):
        #优先连接分量
        if len(comps) > 1:
            return f"{comps[0][0]} — {comps[1][0]}（连通分量）"
        #再配对奇度
        if len(odd_vertices) >= 2:
            return f"{odd_vertices[0]} — {odd_vertices[1]}（配对奇度）"
        return "任意两节点（保持连通）"

    def _suggest_edge_directed(self, active, comps, diff, pos, neg):
        #优先连通：从第一个分量首节点指向第二分量首节点
        if len(comps) > 1:
            return f"{comps[0][0]} → {comps[1][0]}（连通分量）"
        #平衡出入度：从出度过剩到入度不足
        if pos and neg:
            return f"{pos[0]} → {neg[0]}（平衡出入度）"
        #兜底
        if len(active) >= 2:
            return f"{active[0]} → {active[1]}（补边尝试）"
        return "添加一条有向边"

    def _analyze_hamiltonian_directed(self):
        active, indeg, outdeg, degree_sum = self._active_vertices_directed()
        n = len(active)
        if n == 0:
            return "哈密顿（有向）：否（无活跃节点）"
        
        comps = self._components(active)
        base_need = max(0, len(comps) - 1)
        low_deg = [v for v in active if min(indeg[v], outdeg[v]) == 0]
        missing_deg = sum(max(0, 1 - indeg[v]) + max(0, 1 - outdeg[v]) for v in active)
        est_need = base_need + max(0, (missing_deg // 2))

        if n <= 20:
            has_path, path = self._hamiltonian_path_dp(active, directed=True)
            has_cycle, cycle = self._hamiltonian_cycle_dp(active, directed=True)
            if has_path:
                path_str = " → ".join(map(str, path))
                if has_cycle:
                    cycle_str = " → ".join(map(str, cycle + [cycle[0]]))
                    return (
                        "哈密顿（有向）：是（存在通路与回路）\n"
                        f"  → 通路：{path_str}\n"
                        f"  → 回路：{cycle_str}"
                    )
                else:
                    return (
                        "哈密顿（有向）：半哈密顿（有通路，无回路）\n"
                        f"  → 通路：{path_str}"
                    )
            else:
                return (
                    f"哈密顿（有向）：否（未找到有向通路）\n"
                    f"  → 活跃顶点={n}，入或出度为0的顶点={len(low_deg)}\n"
                    f"  → 估算：至少加{max(1, est_need)}条边提升可能性"
                )
        else:
            return (
                f"哈密顿（有向）：未精确检测（顶点数{n}>20）\n"
                f"  → 入或出度为0的顶点={len(low_deg)}，连通分量={len(comps)}\n"
                f"  → 估算：至少加{max(1, est_need)}条边提升可能性"
            )

    def _analyze_hamiltonian_undirected(self):
        active, degrees = self._active_vertices_undirected()
        n = len(active)
        if n == 0:
            return "哈密顿（无向）：否（无活跃节点）"

        comps = self._components(active)
        base_need = max(0, len(comps) - 1)
        low_deg = [v for v in active if degrees[v] < 2]
        missing_deg = sum(max(0, 2 - degrees[v]) for v in active)
        est_need = base_need + (missing_deg // 2)

        if n <= 20:
            has_path, path = self._hamiltonian_path_dp(active, directed=False)
            has_cycle, cycle = self._hamiltonian_cycle_dp(active, directed=False)
            if has_path:
                path_str = " → ".join(map(str, path))
                if has_cycle:
                    cycle_str = " → ".join(map(str, cycle + [cycle[0]]))
                    return (
                        "哈密顿（无向）：是（存在通路与回路）\n"
                        f"  → 通路：{path_str}\n"
                        f"  → 回路：{cycle_str}"
                    )
                else:
                    return (
                        "哈密顿（无向）：半哈密顿（有通路，无回路）\n"
                        f"  → 通路：{path_str}"
                    )
            else:
                return (
                    f"哈密顿（无向）：否（未找到通路）\n"
                    f"  → 活跃顶点={n}，度<2的顶点={len(low_deg)}\n"
                    f"  → 估算：最少加{max(1, est_need)}条边提升可能性"
                )
        else:
            return (
                f"哈密顿（无向）：未精确检测（顶点数{n}>20）\n"
                f"  → 度<2的顶点={len(low_deg)}，连通分量={len(comps)}\n"
                f"  → 估算：最少加{max(1, est_need)}条边提升可能性"
            )

    def _hamiltonian_path_dp(self, active_vertices, directed=True):
        """DP求解哈密顿通路（状态：mask表示访问的节点，u表示当前节点）"""
        idx_map = {v: i for i, v in enumerate(active_vertices)}
        n = len(active_vertices)
        if n == 0:
            return False, []
        
        #构建邻接表（仅活跃节点）
        neighbors = [[] for _ in range(n)]
        for v in active_vertices:
            i = idx_map[v]
            for nb, cnt in self.adj[v].items():
                if cnt > 0 and nb != v and nb in idx_map:
                    neighbors[i].append(idx_map[nb])
            if not directed:
                #对无向图，确保双向邻接
                for nb, cnt in self.adj[v].items():
                    if cnt > 0 and nb != v and nb in idx_map:
                        neighbors[idx_map[nb]].append(i)
        
        #DP[mask][u] = 是否能从起点到u，且访问了mask中的节点
        dp = [[False] * n for _ in range(1 << n)]
        prev = [[-1] * n for _ in range(1 << n)]  #记录路径
        
        #初始化：单个节点
        for i in range(n):
            dp[1 << i][i] = True
        
        #状态转移
        for mask in range(1 << n):
            for u in range(n):
                if not dp[mask][u]:
                    continue
                #遍历邻居
                for v in neighbors[u]:
                    if not (mask & (1 << v)):
                        new_mask = mask | (1 << v)
                        if not dp[new_mask][v]:
                            dp[new_mask][v] = True
                            prev[new_mask][v] = u
        
        #找完整路径
        full_mask = (1 << n) - 1
        for u in range(n):
            if dp[full_mask][u]:
                #回溯路径
                path = []
                cur_mask = full_mask
                cur_u = u
                while cur_mask != 0:
                    path.append(active_vertices[cur_u])
                    next_u = prev[cur_mask][cur_u]
                    cur_mask ^= (1 << cur_u)
                    cur_u = next_u
                return True, path[::-1]
        
        return False, []

    def _hamiltonian_cycle_dp(self, active_vertices, directed=True):
        """DP判断是否存在哈密顿回路，并返回一条回路"""
        idx_map = {v: i for i, v in enumerate(active_vertices)}
        n = len(active_vertices)
        if n == 0:
            return False, []
        neighbors = [[] for _ in range(n)]
        for v in active_vertices:
            i = idx_map[v]
            for nb, cnt in self.adj[v].items():
                if cnt > 0 and nb != v and nb in idx_map:
                    neighbors[i].append(idx_map[nb])
            if not directed:
                for nb, cnt in self.adj[v].items():
                    if cnt > 0 and nb != v and nb in idx_map:
                        neighbors[idx_map[nb]].append(i)

        start = 0
        full_mask = (1 << n) - 1
        dp = [[False] * n for _ in range(1 << n)]
        prev = [[-1] * n for _ in range(1 << n)]
        dp[1 << start][start] = True

        for mask in range(1 << n):
            for u in range(n):
                if not dp[mask][u]:
                    continue
                for v in neighbors[u]:
                    if mask & (1 << v):
                        continue
                    new_mask = mask | (1 << v)
                    if not dp[new_mask][v]:
                        dp[new_mask][v] = True
                        prev[new_mask][v] = u

        for end in range(n):
            if end == start:
                continue
            if dp[full_mask][end] and start in neighbors[end]:
                #回溯回路
                path = [end]
                cur_mask = full_mask
                cur = end
                while cur != start:
                    p = prev[cur_mask][cur]
                    cur_mask ^= (1 << cur)
                    cur = p
                    path.append(cur)
                path.reverse()
                return True, [active_vertices[i] for i in path]
        return False, []

    def _analyze_euler_directed_with_path(self):
        """有向欧拉分析，返回消息和路径"""
        active, indeg, outdeg, degree_sum = self._active_vertices_directed()
        if all(d == 0 for d in degree_sum.values()):
            return {'msg': "欧拉（有向）：否（无任何边，无法构成通路/回路）", 'path': [], 'type': None}
        
        comps = self._components(active)
        k = len(comps)
        diff = {v: outdeg[v] - indeg[v] for v in active}
        pos = [v for v, d in diff.items() if d > 0]
        neg = [v for v, d in diff.items() if d < 0]
        total_pos = sum(diff[v] for v in pos)
        
        has_circuit = k == 1 and total_pos == 0
        has_trail = False
        if k == 1:
            if total_pos == 0:
                has_trail = True
            elif total_pos == 1 and len(pos) == 1 and len(neg) == 1:
                has_trail = True
        
        edges_for_connect = max(0, k - 1)
        need_circuit = max(edges_for_connect, total_pos)
        need_trail = max(edges_for_connect, max(0, total_pos - 1))
        suggest = self._suggest_edge_directed(active, comps, diff, pos, neg)
        
        path_vertices, path_edges = [], []
        path_edges_info = []  #[(u, v, k), ...]
        if has_circuit or has_trail:
            path_vertices, path_edges = self._find_euler_directed_path(active, indeg, outdeg, prefer_circuit=has_circuit)
            #将路径转换为边信息
            if path_vertices:
                path_edges_info = self._vertices_to_edges(path_vertices, path_edges, active)
        
        if has_circuit:
            msg = f"欧拉（有向）：是（存在有向欧拉回路）\n  → 连通分量={k}（按无向视角），全部顶点 in=out"
            if path_vertices:
                path_str = " → ".join(map(str, path_vertices))
                msg += f"\n  → 示例路径：{path_str}"
            return {'msg': msg, 'path': path_edges_info, 'vertices': path_vertices, 'type': 'circuit'}
        if has_trail:
            msg = f"欧拉（有向）：半欧拉（有通路，无回路）\n  → 连通分量={k}，出入度差满足通路条件"
            if path_vertices:
                path_str = " → ".join(map(str, path_vertices))
                msg += f"\n  → 示例路径：{path_str}"
            return {'msg': msg, 'path': path_edges_info, 'vertices': path_vertices, 'type': 'trail'}
        return {'msg': f"欧拉（有向）：否\n  → 连通分量={k}，出入度不平衡顶点 {len(pos)+len(neg)} 个\n  → 缺 {need_trail} 条边可成半欧拉（通路），缺 {need_circuit} 条边可成欧拉（回路）\n  → 建议先加：{suggest}", 'path': [], 'type': None}
    
    def _analyze_euler_undirected_with_path(self):
        """无向欧拉分析，返回消息和路径"""
        active, degrees = self._active_vertices_undirected()
        if all(d == 0 for d in degrees.values()):
            return {'msg': "欧拉（无向）：否（无任何边，无法构成通路/回路）", 'path': [], 'type': None}
        
        comps = self._components(active)
        k = len(comps)
        odd_vertices = [v for v in active if degrees[v] % 2 == 1]
        o = len(odd_vertices)
        
        has_circuit = k == 1 and o == 0
        has_trail = k == 1 and o in (0, 2)
        
        edges_for_connect = max(0, k - 1)
        paired_by_connect = min(edges_for_connect, o // 2)
        remaining_odd = o - paired_by_connect * 2
        need_trail = edges_for_connect + max(0, (remaining_odd - 2) // 2)
        need_circuit = edges_for_connect + (remaining_odd // 2)
        suggest = self._suggest_edge_undirected(comps, odd_vertices)
        
        path_vertices, path_edges = [], []
        path_edges_info = []
        if has_circuit or has_trail:
            path_vertices, path_edges = self._find_euler_undirected_path(active, has_circuit, degrees)
            if path_vertices:
                path_edges_info = self._vertices_to_edges(path_vertices, path_edges, active)
        
        if has_circuit:
            msg = f"欧拉（无向）：是（存在欧拉回路）\n  → 连通分量={k}，奇度顶点={o}"
            if path_vertices:
                path_str = " → ".join(map(str, path_vertices))
                msg += f"\n  → 示例路径：{path_str}"
            return {'msg': msg, 'path': path_edges_info, 'vertices': path_vertices, 'type': 'circuit'}
        if has_trail:
            msg = f"欧拉（无向）：半欧拉（有通路，无回路）\n  → 连通分量={k}，奇度顶点={o}"
            if path_vertices:
                path_str = " → ".join(map(str, path_vertices))
                msg += f"\n  → 示例路径：{path_str}"
            return {'msg': msg, 'path': path_edges_info, 'vertices': path_vertices, 'type': 'trail'}
        return {'msg': f"欧拉（无向）：否\n  → 连通分量={k}，奇度顶点={o}\n  → 缺 {need_trail} 条边可成半欧拉（通路），缺 {need_circuit} 条边可成欧拉（回路）\n  → 建议先加：{suggest}", 'path': [], 'type': None}
    
    def _analyze_hamiltonian_with_path(self):
        """哈密顿分析，返回消息和路径"""
        if self.directed.get():
            return self._analyze_hamiltonian_directed_with_path()
        return self._analyze_hamiltonian_undirected_with_path()
    
    def _analyze_hamiltonian_directed_with_path(self):
        """有向哈密顿分析，返回消息和路径"""
        active, indeg, outdeg, degree_sum = self._active_vertices_directed()
        n = len(active)
        if n == 0:
            return {'msg': "哈密顿（有向）：否（无活跃节点）", 'path': [], 'cycle': [], 'type': None}
        
        comps = self._components(active)
        base_need = max(0, len(comps) - 1)
        low_deg = [v for v in active if min(indeg[v], outdeg[v]) == 0]
        missing_deg = sum(max(0, 1 - indeg[v]) + max(0, 1 - outdeg[v]) for v in active)
        est_need = base_need + max(0, (missing_deg // 2))
        
        if n <= 20:
            has_path, path = self._hamiltonian_path_dp(active, directed=True)
            has_cycle, cycle = self._hamiltonian_cycle_dp(active, directed=True)
            path_edges = []
            cycle_edges = []
            if has_path:
                path_edges = self._vertices_to_hamiltonian_edges(path, active)
            if has_cycle:
                cycle_edges = self._vertices_to_hamiltonian_edges(cycle + [cycle[0]], active)
            
            if has_path:
                path_str = " → ".join(map(str, path))
                if has_cycle:
                    cycle_str = " → ".join(map(str, cycle + [cycle[0]]))
                    return {'msg': f"哈密顿（有向）：是（存在通路与回路）\n  → 通路：{path_str}\n  → 回路：{cycle_str}", 'path': path_edges, 'cycle': cycle_edges, 'vertices': path, 'cycle_vertices': cycle + [cycle[0]], 'type': 'both'}
                else:
                    return {'msg': f"哈密顿（有向）：半哈密顿（有通路，无回路）\n  → 通路：{path_str}", 'path': path_edges, 'cycle': [], 'vertices': path, 'type': 'path'}
            else:
                return {'msg': f"哈密顿（有向）：否（未找到有向通路）\n  → 活跃顶点={n}，入或出度为0的顶点={len(low_deg)}\n  → 估算：至少加{max(1, est_need)}条边提升可能性", 'path': [], 'cycle': [], 'type': None}
        else:
            return {'msg': f"哈密顿（有向）：未精确检测（顶点数{n}>20）\n  → 入或出度为0的顶点={len(low_deg)}，连通分量={len(comps)}\n  → 估算：至少加{max(1, est_need)}条边提升可能性", 'path': [], 'cycle': [], 'type': None}
    
    def _analyze_hamiltonian_undirected_with_path(self):
        """无向哈密顿分析，返回消息和路径"""
        active, degrees = self._active_vertices_undirected()
        n = len(active)
        if n == 0:
            return {'msg': "哈密顿（无向）：否（无活跃节点）", 'path': [], 'cycle': [], 'type': None}
        
        comps = self._components(active)
        base_need = max(0, len(comps) - 1)
        low_deg = [v for v in active if degrees[v] < 2]
        missing_deg = sum(max(0, 2 - degrees[v]) for v in active)
        est_need = base_need + (missing_deg // 2)
        
        if n <= 20:
            has_path, path = self._hamiltonian_path_dp(active, directed=False)
            has_cycle, cycle = self._hamiltonian_cycle_dp(active, directed=False)
            path_edges = []
            cycle_edges = []
            if has_path:
                path_edges = self._vertices_to_hamiltonian_edges(path, active)
            if has_cycle:
                cycle_edges = self._vertices_to_hamiltonian_edges(cycle + [cycle[0]], active)
            
            if has_path:
                path_str = " → ".join(map(str, path))
                if has_cycle:
                    cycle_str = " → ".join(map(str, cycle + [cycle[0]]))
                    return {'msg': f"哈密顿（无向）：是（存在通路与回路）\n  → 通路：{path_str}\n  → 回路：{cycle_str}", 'path': path_edges, 'cycle': cycle_edges, 'vertices': path, 'cycle_vertices': cycle + [cycle[0]], 'type': 'both'}
                else:
                    return {'msg': f"哈密顿（无向）：半哈密顿（有通路，无回路）\n  → 通路：{path_str}", 'path': path_edges, 'cycle': [], 'vertices': path, 'type': 'path'}
            else:
                return {'msg': f"哈密顿（无向）：否（未找到通路）\n  → 活跃顶点={n}，度<2的顶点={len(low_deg)}\n  → 估算：最少加{max(1, est_need)}条边提升可能性", 'path': [], 'cycle': [], 'type': None}
        else:
            return {'msg': f"哈密顿（无向）：未精确检测（顶点数{n}>20）\n  → 度<2的顶点={len(low_deg)}，连通分量={len(comps)}\n  → 估算：最少加{max(1, est_need)}条边提升可能性", 'path': [], 'cycle': [], 'type': None}
    
    def _vertices_to_edges(self, path_vertices, path_edge_labels, active):
        """将顶点路径和边标签转换为边信息列表"""
        if not path_vertices or len(path_vertices) < 2:
            return []
        edges = []
        active_set = set(active)
        for i in range(len(path_vertices) - 1):
            u, v = path_vertices[i], path_vertices[i + 1]
            if u not in active_set or v not in active_set:
                continue
            #查找对应的边（考虑平行边和无向图）
            found = False
            if u in self.adj and v in self.adj[u]:
                cnt = self.adj[u][v]
                #尝试匹配边标签
                label = path_edge_labels[i] if i < len(path_edge_labels) else ""
                k = 1
                if label:
                    #根据标签找到对应的平行边索引
                    for test_k in range(1, cnt + 1):
                        key = self._edge_label_key(u, v, test_k)
                        if self.edge_labels.get(key) == label:
                            k = test_k
                            break
                edges.append((u, v, k))
                found = True
            elif not self.directed.get() and v in self.adj and u in self.adj[v]:
                #无向图，检查反向
                cnt = self.adj[v][u]
                label = path_edge_labels[i] if i < len(path_edge_labels) else ""
                k = 1
                if label:
                    for test_k in range(1, cnt + 1):
                        key = self._edge_label_key(v, u, test_k)
                        if self.edge_labels.get(key) == label:
                            k = test_k
                            break
                #统一使用 (min, max) 格式
                a, b = min(u, v), max(u, v)
                edges.append((a, b, k))
                found = True
            if not found:
                #如果找不到，尝试使用第一条边
                if u in self.adj and v in self.adj[u]:
                    edges.append((u, v, 1))
                elif not self.directed.get() and v in self.adj and u in self.adj[v]:
                    a, b = min(u, v), max(u, v)
                    edges.append((a, b, 1))
        return edges
    
    def _vertices_to_hamiltonian_edges(self, path_vertices, active):
        """将哈密顿路径转换为边信息列表"""
        if not path_vertices or len(path_vertices) < 2:
            return []
        edges = []
        active_set = set(active)
        for i in range(len(path_vertices) - 1):
            u, v = path_vertices[i], path_vertices[i + 1]
            if u not in active_set or v not in active_set:
                continue
            if u in self.adj and v in self.adj[u]:
                #对于无向图，统一使用 (min, max) 格式
                if not self.directed.get():
                    a, b = min(u, v), max(u, v)
                    edges.append((a, b, 1))
                else:
                    edges.append((u, v, 1))
            elif not self.directed.get() and v in self.adj and u in self.adj[v]:
                a, b = min(u, v), max(u, v)
                edges.append((a, b, 1))
        return edges
    
    def _check_graph(self):
        """综合检查欧拉/哈密顿特性"""
        euler_result = self._analyze_euler_with_path()
        ham_result = self._analyze_hamiltonian_with_path()
        euler_msg = euler_result['msg']
        ham_msg = ham_result['msg']
        full_msg = f"{euler_msg}\n\n{ham_msg}"
        
        #更新状态提示
        short_euler = euler_msg.split("\n")[0]
        short_ham = ham_msg.split("\n")[0]
        self.status_var.set(f"{short_euler} | {short_ham}")
        
        #创建自定义结果窗口（带动画功能）
        self._show_result_window(full_msg, euler_result, ham_result)

    def _show_result_window(self, full_msg, euler_result, ham_result):
        """显示自定义结果窗口（带动画功能）"""
        win = tk.Toplevel(self.root)
        win.title("图论分析结果")
        win.geometry("600x500")
        
        #文本显示区域
        text_frame = tk.Frame(win)
        text_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        text_widget = tk.Text(text_frame, wrap=tk.WORD, font=("Arial", 10))
        text_widget.pack(fill=tk.BOTH, expand=True)
        text_widget.insert("1.0", full_msg)
        text_widget.config(state=tk.DISABLED)
        
        #动画控制区域
        anim_frame = tk.Frame(win)
        anim_frame.pack(fill=tk.X, padx=10, pady=5)
        
        #选择要播放的路径
        path_options = []
        if euler_result['path']:
            if euler_result['type'] == 'circuit':
                path_options.append(("欧拉回路", 'euler', 'circuit'))
            elif euler_result['type'] == 'trail':
                path_options.append(("欧拉通路", 'euler', 'trail'))
        if ham_result['path']:
            path_options.append(("哈密顿通路", 'hamilton', 'path'))
        if ham_result.get('cycle'):
            path_options.append(("哈密顿回路", 'hamilton', 'cycle'))
        
        selected_path = tk.StringVar()
        if path_options:
            selected_path.set(path_options[0][0])
            path_menu = tk.OptionMenu(anim_frame, selected_path, *[opt[0] for opt in path_options])
            path_menu.pack(side=tk.LEFT, padx=5)
        else:
            tk.Label(anim_frame, text="无可用路径", fg="gray").pack(side=tk.LEFT, padx=5)
            selected_path = None
        
        #速度调节
        tk.Label(anim_frame, text="速度:").pack(side=tk.LEFT, padx=(10, 2))
        speed_var = tk.IntVar(value=self.animation_speed)
        speed_scale = tk.Scale(anim_frame, from_=100, to=2000, orient=tk.HORIZONTAL, 
                              variable=speed_var, length=150, showvalue=True)
        speed_scale.pack(side=tk.LEFT, padx=5)
        
        #动画控制按钮
        btn_frame = tk.Frame(win)
        btn_frame.pack(fill=tk.X, padx=10, pady=5)
        
        def start_animation():
            if not path_options or not selected_path:
                return
            #找到选中的路径
            selected_name = selected_path.get()
            path_data = None
            vertices_data = None
            anim_type = None
            for opt in path_options:
                if opt[0] == selected_name:
                    anim_type = opt[1]
                    if opt[1] == 'euler':
                        path_data = euler_result['path']
                        vertices_data = euler_result.get('vertices', [])
                    else:
                        if opt[2] == 'path':
                            path_data = ham_result['path']
                            vertices_data = ham_result.get('vertices', [])
                        else:
                            path_data = ham_result.get('cycle', [])
                            vertices_data = ham_result.get('cycle_vertices', [])
                    break
            
            if path_data:
                self.animation_speed = speed_var.get()
                self._start_path_animation(path_data, anim_type, vertices_data)
                start_btn.config(state=tk.DISABLED)
                stop_btn.config(state=tk.NORMAL)
        
        def stop_animation():
            self._stop_path_animation()
            start_btn.config(state=tk.NORMAL)
            stop_btn.config(state=tk.DISABLED)
        
        start_btn = tk.Button(btn_frame, text="播放动画", command=start_animation, 
                             state=tk.NORMAL if path_options else tk.DISABLED)
        start_btn.pack(side=tk.LEFT, padx=5)
        
        stop_btn = tk.Button(btn_frame, text="停止动画", command=stop_animation, 
                            state=tk.DISABLED)
        stop_btn.pack(side=tk.LEFT, padx=5)
        
        tk.Button(btn_frame, text="关闭", command=win.destroy).pack(side=tk.RIGHT, padx=5)
        
        #窗口关闭时停止动画
        def on_close():
            self._stop_path_animation()
            win.destroy()
        win.protocol("WM_DELETE_WINDOW", on_close)
    
    def _start_path_animation(self, path_edges, anim_type, vertices=None):
        """开始路径动画"""
        if not path_edges:
            print("No path_edges provided")
            return
        self._stop_path_animation()  #停止之前的动画
        self.animation_path = path_edges
        self.animation_vertices = vertices if vertices else []
        self.animation_type = anim_type
        self.animation_index = 0
        self.animation_phase = 'vertex' if anim_type == 'euler' and vertices else 'edge'
        self.animation_running = True
        self.vertex_scale = 1.0
        self.vertex_scale_direction = 'up'  #从放大开始
        self.highlighted_vertex = None
        self.highlighted_edges.clear()
        #确保路径数据有效
        if anim_type == 'euler' and vertices:
            #对于欧拉图，顶点数应该比边数多1（通路）或相等（回路，最后一个顶点回到起点）
            if not path_edges:
                #如果没有边路径，可能是数据问题
                return
            #验证数据：对于通路，顶点数 = 边数 + 1；对于回路，顶点数 = 边数 + 1（最后一个回到起点）
            is_circuit = len(vertices) > 1 and vertices[0] == vertices[-1]
            if is_circuit:
                #回路：顶点数应该等于边数+1（最后一个顶点回到起点）
                if len(vertices) != len(path_edges) + 1:
                    #数据不匹配，但继续执行（可能是数据问题）
                    pass
            else:
                #通路：顶点数应该等于边数+1
                if len(vertices) != len(path_edges) + 1:
                    #数据不匹配，但继续执行（可能是数据问题）
                    pass
        self._animate_next_step()
    
    def _stop_path_animation(self):
        """停止路径动画"""
        self.animation_running = False
        if self.animation_after_id:
            self.root.after_cancel(self.animation_after_id)
            self.animation_after_id = None
        self.highlighted_edges.clear()
        self.highlighted_vertex = None
        self.vertex_scale = 1.0
        self.vertex_scale_direction = 'up'
        self._draw_all()
    
    def _animate_next_step(self):
        """播放下一步动画"""
        if not self.animation_running:
            return
        
        #欧拉图：先闪烁顶点，再绘制边
        if self.animation_type == 'euler' and self.animation_vertices:
            if self.animation_phase == 'vertex':
                #顶点闪烁阶段
                #检查是否还有顶点需要处理
                if self.animation_index >= len(self.animation_vertices):
                    #所有顶点处理完毕，动画结束
                    self.animation_running = False
                    self.highlighted_vertex = None
                    self.vertex_scale = 1.0
                    self.highlighted_edges.clear()
                    self._draw_all()
                    return
                
                self.highlighted_vertex = self.animation_vertices[self.animation_index]
                #闪烁动画：放大再缩小
                if self.vertex_scale_direction == 'up':
                    #放大阶段
                    self.vertex_scale = min(1.5, self.vertex_scale + 0.15)
                    self._draw_all()
                    if self.vertex_scale >= 1.5:
                        #放大完成，切换到缩小阶段
                        self.vertex_scale_direction = 'down'
                    self.animation_after_id = self.root.after(50, self._animate_next_step)
                else:
                    #缩小阶段
                    self.vertex_scale = max(1.0, self.vertex_scale - 0.15)
                    self._draw_all()
                    #使用 <= 1.0 或接近 1.0 来检查是否完成闪烁
                    if abs(self.vertex_scale - 1.0) < 0.01 or self.vertex_scale <= 1.0:
                        #闪烁完成（vertex_scale == 1.0），检查是否有对应的边
                        self.vertex_scale = 1.0
                        #第i个顶点对应第i条边（从顶点i到顶点i+1）
                        edge_idx = self.animation_index
                        #确保 animation_path 不为空
                        if not self.animation_path:
                            #没有边路径，动画结束
                            self.animation_running = False
                            self.highlighted_vertex = None
                            self.highlighted_edges.clear()
                            self._draw_all()
                            return
                        
                        #判断是否是回路：顶点路径的第一个和最后一个相同
                        is_circuit = (len(self.animation_vertices) > 1 and 
                                     self.animation_vertices[0] == self.animation_vertices[-1])
                        
                        #对于通路：n个顶点有n-1条边，最后一个顶点（索引n-1）没有边
                        #对于回路：n+1个顶点（最后一个回到起点）有n条边，最后一个顶点（索引n）对应最后一条边（索引n-1）
                        target_edge = None
                        
                        if is_circuit:
                            #回路：第i个顶点对应第i条边（i < len(animation_path)）
                            #对于回路，顶点数 = 边数 + 1（最后一个顶点回到起点）
                            if edge_idx < len(self.animation_path):
                                target_edge = self.animation_path[edge_idx]
                        else:
                            #通路：第i个顶点对应第i条边（i < len(animation_path)）
                            #对于通路，顶点数 = 边数 + 1（最后一个顶点没有边）
                            if edge_idx < len(self.animation_path):
                                target_edge = self.animation_path[edge_idx]
                        if target_edge is not None:
                            #有边，进入边绘制阶段
                            self.animation_phase = 'edge'
                            self.highlighted_edges.clear()
                            #确保 target_edge 是元组格式，并添加到高亮集合
                            if isinstance(target_edge, tuple):
                                self.highlighted_edges.add(target_edge)
                            else:
                                #如果不是元组，尝试转换或使用默认值
                                print(f"Warning: target_edge is not a tuple: {target_edge}")
                            #保持当前顶点高亮（显示从当前顶点出发的边）
                            self._draw_all()
                            #等待一段时间后进入下一个顶点
                            self.animation_after_id = self.root.after(self.animation_speed, self._animate_next_step)
                        else:
                            #没有边了（通路的最后一个顶点），动画结束
                            self.animation_running = False
                            self.highlighted_vertex = None
                            self.highlighted_edges.clear()
                            self._draw_all()
                    else:
                        #继续缩小
                        self.animation_after_id = self.root.after(50, self._animate_next_step)
            else:
                #边绘制完成，移动到下一个顶点
                self.highlighted_edges.clear()
                #清除当前顶点高亮
                self.highlighted_vertex = None
                self.animation_index += 1
                self.animation_phase = 'vertex'
                self.vertex_scale = 1.0
                self.vertex_scale_direction = 'up'  #重置为放大阶段
                #检查是否还有顶点需要处理
                if self.animation_index >= len(self.animation_vertices):
                    #所有顶点处理完毕，动画结束
                    self.animation_running = False
                    self.highlighted_vertex = None
                    self.highlighted_edges.clear()
                    self._draw_all()
                    return
                #重绘清除高亮，然后立即进入下一个顶点的闪烁
                self._draw_all()
                #使用 after 而不是直接递归，避免可能的调用栈问题
                self.animation_after_id = self.root.after(10, self._animate_next_step)
        else:
            #哈密顿图：直接高亮边
            if self.animation_index >= len(self.animation_path):
                self.animation_running = False
                self.highlighted_edges.clear()
                self._draw_all()
                return
            
            #清除之前的高亮
            self.highlighted_edges.clear()
            
            #高亮当前边
            current_edge = self.animation_path[self.animation_index]
            self.highlighted_edges.add(current_edge)
            
            #重绘
            self._draw_all()
            
            #移动到下一条边
            self.animation_index += 1
            
            #安排下一次动画
            if self.animation_running:
                self.animation_after_id = self.root.after(self.animation_speed, self._animate_next_step)
    
    def _draw_edge(self, i, j, cnt):
        """绘制边（普通边/平行边/自环）"""
        if i == j:
            self._draw_self_loop(i, cnt)
            return
        
        x1, y1 = self.nodes[i]
        x2, y2 = self.nodes[j]
        dx, dy = x2 - x1, y2 - y1
        length = hypot(dx, dy)
        if length == 0:
            return
        
        #单位向量（方向/垂直）
        ux, uy = dx / length, dy / length
        px, py = -uy, ux  #垂直方向
        
        arrow_flag = tk.LAST if self.directed.get() else None
        trim = self.node_radius + 6  #让箭头停在节点外侧
        #检测是否穿过其他节点，若有则整体弯曲避让
        def crosses_other_nodes():
            #使用修剪后的线段，避免与端点自身重叠；半径阈值稍放大
            trim_check = self.node_radius + 6
            if length <= 2 * trim_check:
                return False
            cx1 = x1 + ux * trim_check
            cy1 = y1 + uy * trim_check
            cx2 = x2 - ux * trim_check
            cy2 = y2 - uy * trim_check
            seg_dx, seg_dy = cx2 - cx1, cy2 - cy1
            seg_len2 = seg_dx * seg_dx + seg_dy * seg_dy
            if seg_len2 == 0:
                return False
            radius = self.node_radius * 1.4
            for k, (nx, ny) in enumerate(self.nodes):
                if k in (i, j) or nx is None:
                    continue
                vx, vy = nx - cx1, ny - cy1
                t = max(0.0, min(1.0, (vx * seg_dx + vy * seg_dy) / seg_len2))
                projx = cx1 + t * seg_dx
                projy = cy1 + t * seg_dy
                if hypot(nx - projx, ny - projy) <= radius:
                    return True
            return False

        #基础偏移
        if crosses_other_nodes():
            extra = 36
            if cnt == 1:
                offsets = [extra]  #单条也弯曲
            elif cnt == 2:
                offsets = [extra, -extra]
            else:
                offsets = [extra, -extra, 0]
        else:
            offsets = [0, 12, -12][:cnt]
        
        for idx_off, off in enumerate(offsets):
            #检查是否高亮（考虑无向图的情况）
            edge_key = (i, j, idx_off + 1)
            #对于无向图，也检查反向
            if not self.directed.get():
                edge_key_reverse = (j, i, idx_off + 1)
                is_highlighted = edge_key in self.highlighted_edges or edge_key_reverse in self.highlighted_edges
            else:
                is_highlighted = edge_key in self.highlighted_edges
            edge_color = "#ff0000" if is_highlighted else "#444"
            edge_width = 4 if is_highlighted else 2
            
            if off == 0:
                #端点内缩，避免箭头被节点遮挡
                sx, sy = x1 + ux * trim, y1 + uy * trim
                ex, ey = x2 - ux * trim, y2 - uy * trim
                self.canvas.create_line(
                    sx, sy, ex, ey,
                    fill=edge_color, width=edge_width,
                    arrow=arrow_flag,
                    arrowshape=(14, 18, 10),
                    capstyle=tk.ROUND,
                )
                mid_x, mid_y = (sx + ex) / 2, (sy + ey) / 2
            else:
                #弯曲边（平滑）
                bend = off * 1.35
                mx = (x1 + x2) / 2 + px * bend
                my = (y1 + y2) / 2 + py * bend
                #按终段方向内缩尾端，防止箭头被节点遮挡
                tx, ty = ex_dir = (x2 - mx, y2 - my)
                tlen = hypot(tx, ty)
                if tlen > 1e-6:
                    tx, ty = tx / tlen, ty / tlen
                else:
                    tx, ty = ux, uy
                sx, sy = x1 + ux * trim, y1 + uy * trim
                ex, ey = x2 - tx * trim, y2 - ty * trim
                self.canvas.create_line(
                    sx, sy, mx, my, ex, ey,
                    fill=edge_color, width=edge_width, smooth=True,
                    arrow=arrow_flag,
                    arrowshape=(14, 18, 10),
                    capstyle=tk.ROUND,
                )
                mid_x, mid_y = (sx + mx + ex) / 3, (sy + my + ey) / 3

            #边标签
            if self.show_edge_labels.get():
                key = self._edge_label_key(i, j, idx_off + 1)
                label = self.edge_labels.get(key, "")
                if label:
                    self.canvas.create_text(mid_x, mid_y, text=label, fill="#000", font=("Arial", 9, "bold"))

if __name__ == "__main__":
    root = tk.Tk()
    app = GraphApp(root)
    root.mainloop()