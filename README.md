# emqx_persistence_plugin ：Emqx增强插件
## 0.说明
从 `EMQX v4.3.8` 开始, 插件的使用和之前稍微有点区别，因此这里需要注意一下，该插件仅适用于`v4.3.8`。

## 1.简介
本插件是一个EMQX社区版的持久化数据的增强插件，社区出品，非官方。当前支持EMQX4.0以上版本。

## 2.配置
配置文件分为两部分：
1. 持久化的数据源：你要持久化的数据源，一般不要动
2. Mysql连接配置：MySql客户端配置，IP端口，SSL等
## 3.安装
- git clone emqx的源码;
- 切换到 `v4.3.8` tag;
- 首先把代码复制到本地，然后移动到emqx源码目录下的 `apps` 目录下；
- 执行构建。

整个过程需要的命令如下：
```sh
git clone https://github.com/emqx/emqx.git
cd emqx
git fetch origin v4.3.8:v4.3.8
git checkout v4.3.8
cd apps
git clone https://github.com/luxingwen/emqx-persistence-plugin.git
cd ..
make

```
> 注意：本插件不能单独编译，因为依赖了 EMQX 的库。

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

### 4.自动追加ClientId
以 `$WITH_CLIENTID/` 开头的Topic，消息Payload会被添加发送方的ClientId，ClientId会被加到Payload最前面以一个 `#` 分割。
例如A客户端 `c1` 给 `$WITH_CLIENTID/test` 发送了一个消息：`ok` ，如果有客户端订阅，受到的消息为 `c1#ok`。

## 社区
- QQ群：475512169
- 博客：https://wwhai.github.io