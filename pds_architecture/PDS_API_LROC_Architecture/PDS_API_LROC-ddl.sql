-- -----------------------------------------------------
-- Table `dataset_map_image`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `dataset_map_image` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `map_image_id` BIGINT NOT NULL ,
  `dataset_id` BIGINT NOT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `fk_dataset_map_image_map_image` (`map_image_id` ASC) ,
  INDEX `fk_dataset_map_image_dataset` (`dataset_id` ASC) ,
  CONSTRAINT `fk_dataset_map_image_map_image`
    FOREIGN KEY (`map_image_id` )
    REFERENCES `map_image` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_dataset_map_image_dataset`
    FOREIGN KEY (`dataset_id` )
    REFERENCES `dataset` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)


-- -----------------------------------------------------
-- Table `map_image`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `map_image` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(256) NOT NULL ,
  `mission_id` BIGINT NOT NULL ,
  `image_path` VARCHAR(256) NOT NULL ,
  `date` DATETIME NULL ,
  `center_longitude` FLOAT NULL ,
  `center_latitude` FLOAT NULL ,
  `illumination` FLOAT NULL ,
  `camera_angle` FLOAT NULL ,
  `camera_type` VARCHAR(256) NOT NULL ,
  `product_type` VARCHAR(256) NOT NULL ,
  `camera_spec` VARCHAR(256) NOT NULL ,
  PRIMARY KEY (`id`) ,
  CONSTRAINT `fk_mission_map_image`
    FOREIGN KEY (`mission_id` )
    REFERENCES `mission` (`id` )
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
