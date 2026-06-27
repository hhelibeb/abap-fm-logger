[English](./README.md) | **中文**

# abap-fm-logger
函数模块（RFC）的日志记录器

像处理 IDoc 一样处理函数模块。

ABAP 版本：740 及以上

## 设计目标
- 将函数模块日志统一保存到一张通用表中。
- 以 JSON 格式存储参数。
- 通过日志 ID 重新处理记录，类似 WE19。
- 按函数模块和字段值搜索日志，类似 WE10。

## 日志报表
报表 `ZAFL_VIEWER` 可用于搜索日志并重新处理指定记录。
![log report](https://github.com/hhelibeb/helloworld/blob/master/log%20report.png)

## 如何使用

1，引入 include 文件 `ZAFL_MACROS`，其中包含日志器的核心功能。
```abap
FUNCTION-POOL zzxxxx.
INCLUDE zafl_macros.
```
2，根据需要在函数中调用宏。
```abap
FUNCTION z_fm.

**初始化日志器。应始终放在函数开头。
  /afl/log_init.

**可选，最多可指定 3 个用于搜索的字段。
  /afl/set_custom_fields '2020' '1001' '2000000001'.

**可选，可保存状态码和消息文本用于搜索。
  /afl/set_status 'S' 'message'.

**保存日志。应在函数退出前的位置调用。
  /afl/save.

ENDFUNCTION.
```

提示：当真实逻辑存在多个退出点、不想在每个出口都写一遍 `/afl/save` 时，可把业务逻辑下沉到一个内部函数模块（如 `Z_FM_INT`），让被日志记录的函数模块仅作为单一出口的壳来调用它——这样末尾一次 `/afl/save` 即可。

## 配置
表 `ZAFL_CONFIG` 提供基本配置。

### 基本设置
- FNAME：函数模块名称
- ENABLED：勾选后启用日志功能。
- EXPORT：勾选后启用导出参数的日志记录。
- IMPORT：勾选后启用导入参数的日志记录。
- TABLE_IN：勾选后启用表参数的日志记录（函数模块开始时）。
- TABLE_OUT：勾选后启用表参数的日志记录（函数模块结束时）。
- CHANGE：勾选后启用变更参数的日志记录（开始与结束均记录）。

### 附加设置
- NO_COMMIT：勾选后，`/afl/save` 中的 COMMIT 语句将被跳过。
- NO_AUTH_CHECK：勾选后，重新处理方法中的权限检查语句将被跳过。

## 日志清理
将报表 `ZAFL_HISTORY_CLEANER` 作为后台作业调度，用于删除历史日志。

## ⚠️ 升级提醒
如果您已安装过旧版本，在安装新版本时，请注意检查已安装的表 `ZAFL_LOG` 是否与最新版本存在差异。差异可能来自：1）您手动修改过本地 `ZAFL_LOG`；2）新版本扩展了新字段或加宽了现有字段的长度。如存在差异，建议手动确认修改内容，以免造成数据问题。