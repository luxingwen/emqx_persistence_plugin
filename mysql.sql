SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;
-- -----------------------------------------------------------
-- Table structure for on_client_connected
-- -----------------------------------------------------------
DROP TABLE IF EXISTS `on_client_connected`;
CREATE TABLE `on_client_connected` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `action` varchar(32) DEFAULT NULL,
  `node` varchar(32) DEFAULT NULL,
  `client_id` varchar(256) DEFAULT NULL,
  `username` varchar(256) DEFAULT NULL,
  `ip` varchar(32) DEFAULT NULL,
  `connected_at` varchar(64),
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 ROW_FORMAT = DYNAMIC;
-- -----------------------------------------------------------
-- Table structure for on_client_disconnected
-- -----------------------------------------------------------
DROP TABLE IF EXISTS `on_client_disconnected`;
CREATE TABLE `on_client_disconnected` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `action` varchar(32) DEFAULT NULL,
  `node` varchar(32) DEFAULT NULL,
  `client_id` varchar(256) DEFAULT NULL,
  `username` varchar(256) DEFAULT NULL,
  `reason` VARCHAR(40) DEFAULT NULL,
  `disconnected_at` varchar(64),
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 ROW_FORMAT = DYNAMIC;
-- -----------------------------------------------------------
-- Table structure for on_client_publish
-- -----------------------------------------------------------
DROP TABLE IF EXISTS `on_client_publish`;
CREATE TABLE `on_client_publish` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `action` varchar(32) DEFAULT NULL,
  `node` varchar(32) DEFAULT NULL,
  `client_id` varchar(256) DEFAULT NULL,
  `username` varchar(256) DEFAULT NULL,
  `host` VARCHAR(40) DEFAULT NULL,
  `msg_id` varchar(32) DEFAULT NULL,
  `topic` TEXT,
  `payload` TEXT,
  `ts` varchar(64),
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 ROW_FORMAT = DYNAMIC;
SET FOREIGN_KEY_CHECKS = 1;