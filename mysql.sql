SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for on_client_connected
-- ----------------------------
DROP TABLE IF EXISTS `on_client_connected`;
CREATE TABLE `on_client_connected` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `client_id` varchar(32) DEFAULT NULL,
  `username` varchar(32) DEFAULT NULL,
  `host` VARCHAR(40) DEFAULT NULL,
  `at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 ROW_FORMAT=DYNAMIC;

-- ----------------------------
-- Table structure for on_client_disconnected
-- ----------------------------
DROP TABLE IF EXISTS `on_client_disconnected`;
CREATE TABLE `on_client_disconnected` (
  `id` int unsigned NOT NULL AUTO_INCREMENT,
  `client_id` varchar(32) DEFAULT NULL,
  `username` varchar(32) DEFAULT NULL,
  `host` varchar(40) DEFAULT NULL,
  `reason` varchar(255) DEFAULT NULL,
  `at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 ROW_FORMAT=DYNAMIC;
-- ----------------------------
-- Table structure for on_client_publish
-- ----------------------------
DROP TABLE IF EXISTS `on_client_publish`;
CREATE TABLE `on_client_publish` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `client_id` varchar(32) DEFAULT NULL,
  `username` varchar(32) DEFAULT NULL,
  `host` VARCHAR(40) DEFAULT NULL,
  `msg_id` varchar(32) DEFAULT NULL,
  `dup` TINYINT(1) DEFAULT 0,
  `retain` TINYINT(1) DEFAULT 0,
  `qos` tinyint(1) DEFAULT 0,
  `tpoic` TEXT,
  `payload` TEXT,
  `ts` TEXT,
  `at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 ROW_FORMAT=DYNAMIC;

SET FOREIGN_KEY_CHECKS = 1;
