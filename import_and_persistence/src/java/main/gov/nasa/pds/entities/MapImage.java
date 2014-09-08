/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.entities;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.topcoder.json.object.JSONObject;

/**
 * <p>
 * This new entity holds LRO image data.
 * </p>
 *
 * Thread Safety: This class is mutable and not thread safe.
 *
 * <p>
 * Version 1.1 changes [NASA PDS API - LROC Update]:
 * <ol>
 * <li>added {@link #MAP_IMAGE_PRODUCT_TYPE}, {@link #MAP_IMAGE_CAMERA_SPEIFICATION}, {@link #productType},
 * {@link #cameraSpecification} and associated getter/setter methods</li>
 * </ol>
 * </p>
 * 
 * @author fivestarwy, caoweiquan322, schmoel
 * @version 1.1
 */
public class MapImage extends NamedEntity implements MetaDataObjectConsumer {
    /**
     * Constant for "MAP_IMAGE_NAME" String.
     */
    private static final String MAP_IMAGE_NAME = "MAP_IMAGE_NAME";

    /**
     * Constant for "MAP_IMAGE_INFORMATION" String.
     */
    private static final String MAP_IMAGE_INFORMATION = "MAP_IMAGE_INFORMATION";

    /**
     * Constant for "MAP_IMAGE_MISSION_ID" String.
     */
    private static final String MAP_IMAGE_MISSION_ID = "MAP_IMAGE_MISSION_ID";

    /**
     * Constant for "MAP_IMAGE_IMAGE_PATH" String.
     */
    private static final String MAP_IMAGE_IMAGE_PATH = "MAP_IMAGE_IMAGE_PATH";

    /**
     * Constant for "MAP_IMAGE_DATE" String.
     */
    private static final String MAP_IMAGE_DATE = "MAP_IMAGE_DATE";

    /**
     * Constant for "MAP_IMAGE_CENTER_LONGITUDE" String.
     */
    private static final String MAP_IMAGE_CENTER_LONGITUDE = "MAP_IMAGE_CENTER_LONGITUDE";

    /**
     * Constant for "MAP_IMAGE_CENTER_LATITUDE" String.
     */
    private static final String MAP_IMAGE_CENTER_LATITUDE = "MAP_IMAGE_CENTER_LATITUDE";

    /**
     * Constant for "MAP_IMAGE_ILLUMINATION" String.
     */
    private static final String MAP_IMAGE_ILLUMINATION = "MAP_IMAGE_ILLUMINATION";

    /**
     * Constant for "MAP_IMAGE_CAMERA_ANGLE" String.
     */
    private static final String MAP_IMAGE_CAMERA_ANGLE = "MAP_IMAGE_CAMERA_ANGLE";

    /**
     * Constant for "MAP_IMAGE_CAMERA_TYPE" String.
     */
    private static final String MAP_IMAGE_CAMERA_TYPE = "MAP_IMAGE_CAMERA_TYPE";

    /**
     * Constant for "MAP_IMAGE_PRODUCT_TYPE" String.
     */
    private static final String MAP_IMAGE_PRODUCT_TYPE = "MAP_IMAGE_PRODUCT_TYPE";

    /**
     * Constant for "MAP_IMAGE_CAMERA_SPECIFICATION" String.
     */
    private static final Object MAP_IMAGE_CAMERA_SPEIFICATION = "MAP_IMAGE_CAMERA_SPEIFICATION";

    /**
     * Id of the mission of this image.
     */
    private long missionId;

    /**
     * The image path.
     */
    private String imagePath;

    /**
     * The date.
     */
    private Date date;

    /**
     * The center longitude.
     */
    private Double centerLongitude;

    /**
     * The center latitude.
     */
    private Double centerLatitude;

    /**
     * The illumination.
     */
    private Double illumination;

    /**
     * The camera angle.
     */
    private Double cameraAngle;

    /**
     * The camera type.
     */
    private CameraType cameraType;

    /** The product type. */
    private ProductType productType;

    /** The camera specifiction */
    private CameraSpecification cameraSpecification;

    /**
     * Empty constructor.
     */
    public MapImage() {
    }

    /**
     * Constructor. Create the instance with name.
     *
     * @param name
     *            the given name to set.
     */
    public MapImage(String name) {
        super(name);
    }

    /**
     * Populates the instance with given metaDataObject.
     *
     * @param metaDataObject
     *            the given object to handle.
     */
    @Override
    public void fromMetadata(MetadataObject metaDataObject) {
        if (metaDataObject == null) {
            return;
        }

        List<Property> properties = metaDataObject.getProperties();
        List<MetadataObject> children = metaDataObject.getChildren();
        Map<String, Property> map = EntityHelper.getProperties(properties, new String[] { MAP_IMAGE_NAME });

        setName(EntityHelper.getPropertyStringValue(map.get(MAP_IMAGE_NAME)));

        MetadataObject mapImageInfo = EntityHelper.findMetadataObject(MAP_IMAGE_INFORMATION, children);
        if (mapImageInfo != null) {
            Map<String, Property> mapImageInfoMap = EntityHelper.getProperties(mapImageInfo.getProperties(),
                    new String[] { MAP_IMAGE_MISSION_ID, MAP_IMAGE_IMAGE_PATH, MAP_IMAGE_DATE,
                            MAP_IMAGE_CENTER_LONGITUDE, MAP_IMAGE_CENTER_LATITUDE, MAP_IMAGE_ILLUMINATION,
                            MAP_IMAGE_CAMERA_ANGLE, MAP_IMAGE_CAMERA_TYPE });
            this.missionId = EntityHelper.getPropertyLongValue(mapImageInfoMap.get(MAP_IMAGE_MISSION_ID));
            this.imagePath = EntityHelper.getPropertyStringValue(mapImageInfoMap.get(MAP_IMAGE_IMAGE_PATH));
            this.date = EntityHelper.getPropertyDateValue(mapImageInfoMap.get(MAP_IMAGE_DATE), "yyyy-MM-dd");
            if (this.date == null) {
                this.date = EntityHelper.getPropertyDateValue(mapImageInfoMap.get(MAP_IMAGE_DATE), "yyyy-MM");
            }
            this.centerLongitude = EntityHelper.getPropertyDoubleValue(mapImageInfoMap.get(MAP_IMAGE_CENTER_LONGITUDE));
            this.centerLatitude = EntityHelper.getPropertyDoubleValue(mapImageInfoMap.get(MAP_IMAGE_CENTER_LATITUDE));
            this.illumination = EntityHelper.getPropertyDoubleValue(mapImageInfoMap.get(MAP_IMAGE_ILLUMINATION));
            this.cameraAngle = EntityHelper.getPropertyDoubleValue(mapImageInfoMap.get(MAP_IMAGE_CAMERA_ANGLE));
            this.cameraType = EntityHelper.getPropertyCameraTypeValue(mapImageInfoMap.get(MAP_IMAGE_CAMERA_TYPE));
            this.productType = EntityHelper.getPropertyProductTypeValue(mapImageInfoMap.get(MAP_IMAGE_PRODUCT_TYPE));
            this.cameraSpecification = EntityHelper.getPropertyCameraSpecificationValue(mapImageInfoMap
                    .get(MAP_IMAGE_CAMERA_SPEIFICATION));
        }
    }

    /**
     * Gets the JSONObject instance.
     *
     * @return the JSONObject for this instance.
     */
    @Override
    public JSONObject toJSONObject() {
        JSONObject object = super.toJSONObject();
        EntityHelper.setLong(object, "missionId", missionId);
        EntityHelper.setString(object, "imagePath", imagePath);
        EntityHelper.setString(object, "date", EntityHelper.convertDateToString(date));
        EntityHelper.setDouble(object, "centerLongitude", centerLongitude);
        EntityHelper.setDouble(object, "centerLatitude", centerLatitude);
        EntityHelper.setDouble(object, "illumination", illumination);
        EntityHelper.setDouble(object, "cameraAngle", cameraAngle);
        EntityHelper.setString(object, "cameraType", EntityHelper.convertCameraTypeToString(cameraType));
        EntityHelper.setString(object, "productType", EntityHelper.convertProductTypeToString(productType));
        EntityHelper.setString(object, "cameraSpecifiction",
                EntityHelper.convertCameraSpecificationToString(cameraSpecification));
        return object;
    }

    /**
     * Gets the missionId value.
     *
     * @return the missionId value.
     */
    public long getMissionId() {
        return missionId;
    }

    /**
     * Sets the given value to missionId.
     *
     * @param missionId
     *            the given value to set.
     */
    public void setMissionId(long missionId) {
        this.missionId = missionId;
    }

    /**
     * Gets the imagePath value.
     *
     * @return the imagePath value.
     */
    public String getImagePath() {
        return imagePath;
    }

    /**
     * Sets the given value to imagePath.
     *
     * @param imagePath
     *            the given value to set.
     */
    public void setImagePath(String imagePath) {
        this.imagePath = imagePath;
    }

    /**
     * Gets the date value.
     *
     * @return the date value.
     */
    public Date getDate() {
        return date;
    }

    /**
     * Sets the given value to date.
     *
     * @param date
     *            the given value to set.
     */
    public void setDate(Date date) {
        this.date = date;
    }

    /**
     * Gets the centerLongitude value.
     *
     * @return the centerLongitude value.
     */
    public Double getCenterLongitude() {
        return centerLongitude;
    }

    /**
     * Sets the given value to centerLongitude.
     *
     * @param centerLongitude
     *            the given value to set.
     */
    public void setCenterLongitude(Double centerLongitude) {
        this.centerLongitude = centerLongitude;
    }

    /**
     * Gets the centerLatitude value.
     *
     * @return the centerLatitude value.
     */
    public Double getCenterLatitude() {
        return centerLatitude;
    }

    /**
     * Sets the given value to centerLatitude.
     *
     * @param centerLatitude
     *            the given value to set.
     */
    public void setCenterLatitude(Double centerLatitude) {
        this.centerLatitude = centerLatitude;
    }

    /**
     * Gets the illumination value.
     *
     * @return the illumination value.
     */
    public Double getIllumination() {
        return illumination;
    }

    /**
     * Sets the given value to illumination.
     *
     * @param illumination
     *            the given value to set.
     */
    public void setIllumination(Double illumination) {
        this.illumination = illumination;
    }

    /**
     * Gets the cameraAngle value.
     *
     * @return the cameraAngle value.
     */
    public Double getCameraAngle() {
        return cameraAngle;
    }

    /**
     * Sets the given value to cameraAngle.
     *
     * @param cameraAngle
     *            the given value to set.
     */
    public void setCameraAngle(Double cameraAngle) {
        this.cameraAngle = cameraAngle;
    }

    /**
     * Gets the cameraType value.
     *
     * @return the cameraType value.
     */
    public CameraType getCameraType() {
        return cameraType;
    }

    /**
     * Sets the given value to cameraType.
     *
     * @param cameraType
     *            the given value to set.
     */
    public void setCameraType(CameraType cameraType) {
        this.cameraType = cameraType;
    }

    public ProductType getProductType() {
        return productType;
    }

    public void setProductType(ProductType productType) {
        this.productType = productType;
    }

    public CameraSpecification getCameraSpecification() {
        return cameraSpecification;
    }

    public void setCameraSpecification(CameraSpecification cameraSpecification) {
        this.cameraSpecification = cameraSpecification;
    }
}
