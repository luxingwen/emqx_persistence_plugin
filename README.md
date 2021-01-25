# emqx_persistence_plugin
Emqx增强插件（非官方）。
## 1.简介
本插件是一个EMQX社区版的持久化数据的增强插件，社区出品，非官方。当前支持EMQX4.0以上版本。
## 2.配置
配置文件分为两部分：
1. 持久化的数据源：你要持久化的数据源，一般不要动
2. Mysql连接配置：MySql客户端配置，IP端口，SSL等
## 3.安装
参考这里：https://docs.emqx.io/broker/latest/cn/advanced/plugins.html
## 4.功能介绍
### 1.Mysql持久化数据
1. 对`$PERSISTENCE/`开头的Topic，发送的任何消息被持久化到Mysql;
2. 客户端上下线记录持久化到Mysql;

### 3.点对点消息
Topic满足该规则可被直接发送给某个客户端，而不需要订阅任何Topic。例如直接给ClientId为`test1`的客户端发送消息，直接在Topic后面加上即可:`$P2P/test1`。

#### 注意事项
- client_id 不可空
- `$P2P/`不可订阅
- ClientId中不可带非法字符:`/ * # +`

## 社区
- QQ群：475512169
- 博客：https://openlab.ezlinker.cn