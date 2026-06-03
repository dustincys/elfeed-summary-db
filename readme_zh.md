# elfeed-summary-db

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Emacs](https://img.shields.io/badge/Emacs-28.1%2B-blue.svg)](https://www.gnu.org/software/emacs/)
[![Python](https://img.shields.io/badge/Python-3.10%2B-blue.svg)](https://www.python.org/)

[English](README.md) | **中文**

**为你的 Elfeed RSS 摘要提供语义搜索——基于向量嵌入。**

elfeed-summary-db 将你为 [Elfeed](https://github.com/skeeto/elfeed) RSS 条目生成的 AI 摘要索引到向量数据库中，实现快速的语义搜索。按*含义*而非关键词查找文章。支持通过 [gptel](https://github.com/karthink/gptel) 集成 LLM 工具，让你的 AI 助手也能搜索你的阅读历史。

## 架构

```
┌─────────────────────────────────────────────────────────┐
│                        Emacs                            │
│  ┌──────────┐  ┌──────────┐  ┌───────────────────────┐ │
│  │  elfeed  │──│  解析    │──│  HTTP 客户端 (plz)    │ │
│  │  条目    │  │  → JSON  │  │  异步索引 + 搜索      │ │
│  └──────────┘  └──────────┘  └───────────┬───────────┘ │
│       │                                  │              │
│  ┌────┴────────┐              ┌──────────┴──────────┐  │
│  │ 保存时自动  │              │  Transient 菜单界面  │  │
│  │ 触发索引    │              │  语义搜索            │  │
│  └─────────────┘              │  Ivy 集成            │  │
│                               │  gptel MCP 工具      │  │
│                               └─────────────────────┘  │
└─────────────────────────────────────┬───────────────────┘
                                      │ HTTP (localhost:8875)
┌─────────────────────────────────────┴───────────────────┐
│                 Python FastAPI 服务                      │
│  ┌──────────┐  ┌────────────┐  ┌────────────────────┐  │
│  │  /health │  │ /api/entry │  │ /api/search/       │  │
│  │  /logs   │  │  (索引)    │  │   semantic         │  │
│  └──────────┘  └────────────┘  └────────┬───────────┘  │
│                                         │               │
│  ┌──────────────────────────────────────┴──────────┐   │
│  │  嵌入服务 (BGE-M3, CLIP)                         │   │
│  │  交叉编码器重排序                                │   │
│  │  文本分块服务                                    │   │
│  └─────────────────────┬───────────────────────────┘   │
│                        │                                │
│  ┌─────────────────────┴───────────────────────────┐   │
│  │  libsql (SQLite + 向量搜索)                      │   │
│  └─────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
```

## 功能特性

### 🔍 语义搜索
- **向量嵌入** 使用 BGE-M3 模型——按含义搜索，而非关键词匹配
- **精确搜索** 适用于小规模数据（<5000 条），**ANN 向量搜索** 适用于大规模数据
- **交叉编码器重排序** 提升结果相关性（可选）
- **Ivy 动态搜索** — 输入时实时更新结果
- **标题筛选** — 限定搜索特定订阅源或模式

### 🤖 LLM 集成 (gptel)
- 为 gptel 暴露 **MCP 风格的工具**
- `elfeed_semantic_search` — 在 LLM 对话中进行向量搜索
- `elfeed_fuzzy_summary_search` — 本地模糊搜索备选方案
- 可点击的 `[[ELFEED:ID]]` 链接，在 gptel 缓冲区中渲染为按钮

### ⚡ 智能索引
- **保存时自动索引** 通过 advice 机制实现
- **基于队列的异步索引** — 非阻塞，Emacs 保持响应
- **进度追踪** 带状态报告
- **恢复/取消** 卡住的操作
- **重建索引** 并清理过期条目
- **可配置的超时和并发**

### 🖥️ Transient 菜单界面
- 通过 `H-v` 或 `M-x elfeed-summary-db` 访问
- 所有命令组织在可发现的菜单中
- 一站式管理：范围控制、服务器管理、索引和搜索

### 🌐 Web 界面
- FastAPI 服务内置首页
- 自动生成的 API 文档（FastAPI Swagger UI）

## 安装

### 环境要求

- **Emacs** 28.1+
- **Python** 3.10+
- **[uv](https://github.com/astral-sh/uv)** — 快速的 Python 包管理器

### 1. Python 后端

```bash
cd python
uv sync
```

这会安装：FastAPI、Uvicorn、sentence-transformers、CLIP、PyTorch、libsql 等。

### 2. Emacs 包

```elisp
;; 克隆仓库
;; git clone https://github.com/dustincys/elfeed-summary-db ~/.emacs.d/private/elfeed-summary-db

;; 添加到 load-path 并加载
(add-to-list 'load-path "~/.emacs.d/private/elfeed-summary-db/elisp")
(require 'elfeed-summary-db)
```

### 3. 依赖包

本包依赖：
- `elfeed` — RSS 阅读器
- `transient` — 菜单界面（Emacs 28+ 内置，或通过 ELPA 安装）
- `plz` — HTTP 客户端（通过 ELPA 安装）
- `compat` — 兼容性库
- `gptel`（可选）— LLM 工具集成
- `ivy`（可选）— 动态输入即搜
- `flx`（可选）— gptel 工具中的模糊搜索

### 4. 配置

```elisp
(use-package elfeed-summary-db
  :load-path "~/.emacs.d/private/elfeed-summary-db/elisp"
  :custom
  ;; 服务器设置
  (elfeed-summary-db-server-host "127.0.0.1")
  (elfeed-summary-db-server-port 8875)

  ;; 需要时自动启动服务
  (elfeed-summary-db-auto-start-server t)

  ;; 保存摘要时自动启用索引
  (elfeed-summary-db-auto-enable t)

  ;; 搜索设置
  (elfeed-summary-db-search-default-limit 10)
  (elfeed-summary-db-search-use-reranking nil)  ; 设为 t 以获得更准确的结果
  (elfeed-summary-db-search-rerank-candidates 50)

  ;; 索引设置
  (elfeed-summary-db-index-delay 0.5)   ; 条目之间的延迟
  (elfeed-summary-db-index-timeout 240) ; 每条目的超时时间

  :bind
  ("H-v" . elfeed-summary-db-menu))
```

## 使用方法

### 启动服务器

当 `elfeed-summary-db-auto-start-server` 为 `t` 时，服务器会在需要时自动启动。也可以手动启动：

- `M-x elfeed-summary-db-start-server` — 启动 Python 后端
- `M-x elfeed-summary-db-stop-server` — 停止后端
- `M-x elfeed-summary-db-restart-server` — 重启
- `M-x elfeed-summary-db-server-status` — 检查运行状态

### 索引条目

当 `elfeed-summary-db-auto-enable` 为 `t` 时，条目 AI 摘要保存后会触发自动索引。

手动索引命令：
- `M-x elfeed-summary-db-index-elfeed` — 索引**所有**含摘要的条目
- `M-x elfeed-summary-db-reindex-database` — 重建索引，清理过期条目
- `M-x elfeed-summary-db-index-current-entry` — 索引当前条目
- `M-x elfeed-summary-db-indexing-status` — 查看进度
- `M-x elfeed-summary-db-cancel-indexing` — 取消当前操作
- `M-x elfeed-summary-db-resume-indexing` — 恢复卡住的索引
- `M-x elfeed-summary-db-show-failed-entries` — 列出索引失败的条目

### 语义搜索

| 命令 | 说明 |
|------|------|
| `M-x elfeed-summary-db-semantic-search` | 按含义搜索（completing-read） |
| `M-x elfeed-summary-db-semantic-search-ivy` | 动态输入即搜（Ivy） |
| `M-x elfeed-summary-db-search-at-point` | 使用光标处或选中区域的文本搜索 |

搜索结果中：
- 显示**相似度分数**、**上下文片段**和**订阅源标题**
- 按 `RET` 在 Elfeed 中打开选中条目
- Ivy 模式下：`o` 打开条目，`c` 复制完整摘要到剪贴板

### Transient 菜单

按 `H-v` 或 `M-x elfeed-summary-db` 打开菜单：

```
elfeed-summary db [范围: 全部条目]
搜索和管理你的 elfeed 条目。
┌─ 范围 ───────────────────────────────────────┐
│ -a  全部条目                                  │
│ -t  标题模式                                  │
├─ 搜索 ───────────────────────────────────────┤
│  v  语义搜索（向量嵌入）                      │
├─ 管理 ───────────────────────────────────────┤
│  u  索引当前条目                              │
│  U  索引全部条目                              │
│  r  重建数据库索引                            │
│  S  服务器状态                                │
│  R  重启服务器                                │
│  L  查看服务器日志                            │
│  W  打开 Web 界面                             │
│  X  清空数据库（不可恢复！）                  │
├─ 操作 ───────────────────────────────────────┤
│  q  退出                                      │
└───────────────────────────────────────────────┘
```

### gptel 集成

注册工具后，你的 LLM 可以搜索阅读历史：

```elisp
;; gptel 加载后执行:
M-x elfeed-summary-db-gptel-register-tools
```

在任何 gptel 对话中，LLM 可以调用：
- **`elfeed_semantic_search`** — "查找关于深度学习优化器的文章"
- **`elfeed_fuzzy_summary_search`** — "搜索提及 BPE 分词的内容"

结果包含可点击的按钮，直接在 Elfeed 中打开条目。

移除工具：
```
M-x elfeed-summary-db-gptel-unregister-tools
```

### Web 界面

```elisp
M-x elfeed-summary-db-open-web-interface
```

在浏览器中打开 `http://127.0.0.1:8875`，首页显示服务器信息和 API 文档链接。

## API 端点

Python 服务暴露以下端点：

| 方法 | 路径 | 说明 |
|------|------|------|
| `GET` | `/health` | 健康检查 |
| `GET` | `/` | Web 首页 |
| `GET` | `/api/logs?lines=N` | 最近的服务器日志 |
| `POST` | `/api/entry` | 索引条目 |
| `DELETE` | `/api/entry?entry-id=ID` | 删除条目 |
| `GET` | `/api/entries` | 列出所有已索引的条目 ID |
| `POST` | `/api/search/semantic` | 语义向量搜索 |
| `GET` | `/api/stats` | 数据库统计信息 |
| `DELETE` | `/api/stats/clear-database` | 清空整个数据库 |
| `POST` | `/api/shutdown` | 优雅关闭服务器 |

## 环境变量

| 变量 | 默认值 | 说明 |
|------|--------|------|
| `ELFEED_SUMMARY_DB_HOST` | `127.0.0.1` | 服务器地址 |
| `ELFEED_SUMMARY_DB_PORT` | `8875` | 服务器端口 |
| `ELFEED_SUMMARY_DB_SEMANTIC_DB_PATH` | `~/elfeed-summary-db/elfeed-summary-db-semantic.db` | SQLite 数据库路径 |

## 工作原理

### 数据流

1. **Elfeed** 抓取 RSS 文章，你的工作流生成 AI 摘要（存储为 `:summary` 元数据）
2. **elfeed-summary-db** 钩入保存函数——摘要保存时，将条目解析为 JSON 并发送到 Python 服务
3. **Python 服务** 将文本分块，通过 BGE-M3 生成嵌入向量，存入 libsql 数据库并建立向量索引
4. **搜索时**，查询文本被嵌入后与存储的向量进行余弦相似度计算
5. 结果返回 Emacs，在 completing-read 界面中展示（Ivy 模式下为动态展示）

### 嵌入模型

- **默认**：BGE-M3（1024 维嵌入向量）
- 多语言高质量嵌入
- 小规模数据使用精确余弦相似度搜索；大规模数据使用 libsql `vector_top_k` ANN 搜索
- 可选的交叉编码器重排序以提升相关性

### 数据库

- 使用 [libsql](https://github.com/tursodatabase/libsql) — 支持原生向量搜索的 SQLite
- 两个主要表：`chunks`（文本分块及元数据）和 `embeddings`（向量嵌入）
- 向量索引用于大规模数据的快速近似最近邻搜索

## 项目结构

```
elfeed-summary-db/
├── README.md
├── README_zh.md                      # 中文 README
├── readme.org                        # 原始 Org-mode README
├── elisp/
│   ├── elfeed-summary-db.el          # 主包、钩子、配置
│   ├── elfeed-summary-db-parse.el    # Elfeed 条目 → JSON 解析
│   ├── elfeed-summary-db-client.el   # 异步 HTTP 客户端、队列管理
│   ├── elfeed-summary-db-server.el   # 服务器生命周期（启动/停止/健康检查）
│   ├── elfeed-summary-db-search.el   # 语义搜索界面 + Ivy 集成
│   ├── elfeed-summary-db-gptel-tools.el  # gptel LLM 工具集成
│   └── elfeed-summary-db-ui.el       # Transient 菜单界面
├── python/
│   ├── pyproject.toml                # Python 依赖
│   ├── elfeed_summary_db_server/
│   │   ├── main.py                   # FastAPI 应用入口
│   │   ├── config.py                 # Pydantic 配置
│   │   ├── api/
│   │   │   ├── indexing.py           # 条目索引端点
│   │   │   ├── search.py             # 语义搜索端点
│   │   │   └── stats.py             # 统计与管理端点
│   │   ├── models/
│   │   │   ├── schemas.py            # Pydantic 请求/响应模型
│   │   │   └── semantic_schema.py    # 向量数据库 SQL 建表语句
│   │   ├── services/
│   │   │   ├── database.py           # libsql 数据库连接
│   │   │   ├── embeddings.py         # BGE-M3 嵌入服务
│   │   │   ├── chunking.py           # 文本分块策略
│   │   │   ├── reranker.py           # 交叉编码器重排序
│   │   │   └── clip_service.py       # CLIP 图片嵌入
│   │   ├── templates/
│   │   └── log_handler.py            # 内存日志缓冲
│   └── scripts/                      # 基准测试和性能分析
├── scripts/
│   └── setup.sh                      # 开发环境初始化
└── tests/
    └── elfeed-summary-db-search-test.el  # ERT 测试
```

## 开发

### 运行测试

**Emacs Lisp 测试：**
```bash
emacs -batch -l ert \
  -l elisp/elfeed-summary-db.el \
  -l tests/elfeed-summary-db-search-test.el \
  -f ert-run-tests-batch-and-exit
```

**Python 测试：**
```bash
cd python
uv run pytest
```

### 手动启动服务器

```bash
cd python
uv run uvicorn elfeed_summary_db_server.main:app --reload --host 127.0.0.1 --port 8875
```

## 许可证

MIT License。

## 作者

Yanshuo Chu — [GitHub](https://github.com/dustincys)

---

<p align="center">
  <sub>为 Emacs 社区用 ❤️ 构建</sub>
</p>
