# emqx_persistence_mysql ：Emqx数据持久化到Mysql
## 0.说明
从 `EMQX v4.3.10` 开始, 插件的使用和之前稍微有点区别，因此这里需要注意一下，该插件仅适用于`v4.3.10+`。

## 1.简介
本插件是一个EMQX社区版的持久化数据的增强插件，社区出品，非官方。当前支持EMQX4.0以上版本。

## 2.配置
配置文件分为两部分：
1. 持久化的数据源：你要持久化的数据源，一般不要动
2. Mysql连接配置：MySql客户端配置，IP端口，SSL等
## 3.安装
- git clone emqx的源码;
- 切换到 `v4.3.10` tag;
- 首先把代码复制到本地，然后移动到emqx源码目录下的 `apps` 目录下；
- 在 `rebar.config.erl`脚本下找到大概290行位置处：`relx_plugin_apps(ReleaseType)` 函数，将该插件加入进去
- 执行`make` 命令构建。

    > 注意：本插件不能单独编译，因为依赖了 EMQX 的库。

## 4.功能介绍
### 1.Mysql持久化
1. 对`$MYSQL`开头的Topic，发送的任何消息被持久化到Mysql;
2. 客户端上下线记录持久化到Mysql;

## 社区
- QQ群：475512169
- 博客：https://wwhai.github.io