# emqx_persistence_plugin
Emqx增强插件（非官方）。
## 1.简介
本插件是一个EMQX的增强插件，社区出品，非官方。当前支持EMQX4.0以上版本。
## 2.配置
配置文件分为两部分：
1. 持久化的数据源：你要持久化的数据源，一般不要动
2. Mysql连接配置：MySql客户端配置，这个就不多说了
## 3.安装
参考这里：https://docs.emqx.io/broker/latest/cn/advanced/plugins.html
## 4.功能介绍
### 1.Mysql持久化数据
1. 对`$PERSISTENCE/`开头的Topic，发送的任何消息被持久化到Mysql;
2. 客户端上下线记录持久化到Mysql;
### 2.Web认证接口
可配置一个Web认证接口，用来给客户端鉴权,当开启认证功能以后，EMQX会把参与认证的客户端JSON信息，提交到WEB接口：
```
 {
    "username":"your-username",
    "password":"your-password",
    "client_id":"your-clientid"
 }
```
WEB接口实现必须返回如下格式：
- 认证成功: `webauth.success`的值
- 认证失败: `webauth.failure`的值
> 注意：默认值是`success`

### 3.点对点消息
Topic满足该规则可被直接发送给某个客户端，而不需要订阅任何Topic:`$P2P/{ClientId}`。例如直接给ClientId为`bgyoujbh67ukbh7u`的客户端发送消息，直接在Topic后面加上即可:`$P2P/bgyoujbh67ukbh7u`。
#### 注意事项
- client_id 不可空
- `$P2P/`不可订阅
- ClientId中不可带非法字符:`/ * # +`
