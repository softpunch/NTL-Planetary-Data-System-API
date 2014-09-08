/*
 * Copyright (C) 2011-2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.entities;

import java.util.Date;
import java.util.List;

import com.topcoder.json.object.JSONObject;

/**
 * <p>
 * This class represents the search criteria for locating data sets.
 * </p>
 *
 * <p>
 * Thread Safety: This class is mutable and not thread safe.
 * </p>
 *
 * <p>
 * Version 1.1 changes [NASA LMMP - PDS API Update Module Assembly]:
 * <ol>
 * <li>Add LRO filter parameters.</li>
 * </ol>
 * </p>
 *
 * <p>
 * Version 1.2 changes [NASA PDS API - LROC Update]:
 * <ol>
 * <li>Add {@link #productType}, {@link #cameraSpecification}</li>
 * </ol>
 * </p>
 * @author TCSASSEMBLER, fivestarwy, caoweiquan322, schmoel
 * @version 1.2
 */
public class SearchCriteria {
    /**
     * Set data set id.
     */
    private long dataSetId;
    /**
     * Represents the targetTypes. It is managed with a getter and setter. It may have any value. It is fully mutable.
     */
    private List<String> targetTypes;
    /**
     * Represents the targets. It is managed with a getter and setter. It may have any value. It is fully mutable.
     */
    private List<String> targets;
    /**
     * Represents the missions. It is managed with a getter and setter. It may have any value. It is fully mutable.
     */
    private List<String> missions;
    /**
     * Represents the It is managed with a getter and setter. It may have any value. It is fully mutable.
     */
    private List<String> instruments;
    /**
     * Represents the instrumentHosts. It is managed with a getter and setter. It may have any value. It is fully
     * mutable.
     */
    private List<String> instrumentHosts;
    /**
     * Represents the startDate. It is managed with a getter and setter. It may have any value. It is fully mutable.
     */
    private Date startDate;
    /**
     * Represents the stopDate. It is managed with a getter and setter. It may have any value. It is fully mutable.
     */
    private Date stopDate;
    
    private String productType;
    
    private String cameraSpecification;


    /* NOTE: do not use this search parameters yet.
    private boolean whiteSpots;
    private Float lowerDataRating;
    private Float upperDataRating;
    */

    /**
     * The flag indicates whether to search LRO data or not.
     */
    private boolean useLRO;

    /**
     * The minimum longitude.
     */
    private Double longitudeMin;

    /**
     * The maximum longitude.
     */
    private Double longitudeMax;

    /**
     * The minimum latitude.
     */
    private Double latitudeMin;

    /**
     * The maximum latitude.
     */
    private Double latitudeMax;

    /**
     * The minimum illumination.
     */
    private Double illuminationMin;

    /**
     * The maximum illumination.
     */
    private Double illuminationMax;

    /**
     * The minimum camera angle.
     */
    private Double cameraAngleMin;

    /**
     * The maximum camera angle.
     */
    private Double cameraAngleMax;

    /**
     * Empty constructor
     */
    public SearchCriteria() {
    }

    // dataSetId property
    public long getDataSetId() {
        return dataSetId;
    }
    public void setDataSetId(long dataSetId) {
        this.dataSetId = dataSetId;
    }

    /**
     * Gets the targetTypes value.
     *
     * @return the targetTypes value.
     */
    public List<String> getTargetTypes() {
        return targetTypes;
    }

    /**
     * Sets the given value to targetTypes.
     *
     * @param targetTypes
     *            the given value to set.
     */
    public void setTargetTypes(List<String> targetTypes) {
        this.targetTypes = targetTypes;
    }

    /**
     * Gets the targets value.
     *
     * @return the targets value.
     */
    public List<String> getTargets() {
        return targets;
    }

    /**
     * Sets the given value to targets.
     *
     * @param targets
     *            the given value to set.
     */
    public void setTargets(List<String> targets) {
        this.targets = targets;
    }

    /**
     * Gets the missions value.
     *
     * @return the missions value.
     */
    public List<String> getMissions() {
        return missions;
    }

    /**
     * Sets the given value to missions.
     *
     * @param missions
     *            the given value to set.
     */
    public void setMissions(List<String> missions) {
        this.missions = missions;
    }

    /**
     * Gets the instruments value.
     *
     * @return the instruments value.
     */
    public List<String> getInstruments() {
        return instruments;
    }

    /**
     * Sets the given value to instruments.
     *
     * @param instruments
     *            the given value to set.
     */
    public void setInstruments(List<String> instruments) {
        this.instruments = instruments;
    }

    /**
     * Gets the startDate value.
     *
     * @return the startDate value.
     */
    public Date getStartDate() {
        return startDate;
    }

    /**
     * Sets the given value to startDate.
     *
     * @param startDate
     *            the given value to set.
     */
    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    /**
     * Gets the stopDate value.
     *
     * @return the stopDate value.
     */
    public Date getStopDate() {
        return stopDate;
    }

    /**
     * Sets the given value to stopDate.
     *
     * @param stopDate
     *            the given value to set.
     */
    public void setStopDate(Date stopDate) {
        this.stopDate = stopDate;
    }

    /**
     * Gets the instrumentHosts value.
     *
     * @return the instrumentHosts value.
     */
    public List<String> getInstrumentHosts() {
        return instrumentHosts;
    }

    /**
     * Sets the given value to instrumentHosts.
     *
     * @param instrumentHosts
     *            the given value to set.
     */
    public void setInstrumentHosts(List<String> instrumentHosts) {
        this.instrumentHosts = instrumentHosts;
    }

/*
    public boolean isWhiteSpots() {
        return whiteSpots;
    }
    public void setWhiteSpots(boolean whiteSpots) {
        this.whiteSpots = whiteSpots;
    }

    public Float getLowerDataRating() {
        return lowerDataRating;
    }
    public void setLowerDataRating(Float lowerDataRating) {
        this.lowerDataRating = lowerDataRating;
    }

    public Float getUpperDataRating() {
        return upperDataRating;
    }
    public void setUpperDataRating(Float upperDataRating) {
        this.upperDataRating = upperDataRating;
    }
*/

    /**
     * Gets the useLRO value.
     *
     * @return the useLRO value.
     */
    public boolean isUseLRO() {
        return useLRO;
    }

    /**
     * Sets the given value to useLRO.
     *
     * @param useLRO
     *            the given value to set.
     */
    public void setUseLRO(boolean useLRO) {
        this.useLRO = useLRO;
    }

    /**
     * Gets the longitudeMin value.
     *
     * @return the longitudeMin value.
     */
    public Double getLongitudeMin() {
        return longitudeMin;
    }

    /**
     * Sets the given value to longitudeMin.
     *
     * @param longitudeMin
     *            the given value to set.
     */
    public void setLongitudeMin(Double longitudeMin) {
        this.longitudeMin = longitudeMin;
    }

    /**
     * Gets the longitudeMax value.
     *
     * @return the longitudeMax value.
     */
    public Double getLongitudeMax() {
        return longitudeMax;
    }

    /**
     * Sets the given value to longitudeMax.
     *
     * @param longitudeMax
     *            the given value to set.
     */
    public void setLongitudeMax(Double longitudeMax) {
        this.longitudeMax = longitudeMax;
    }

    /**
     * Gets the latitudeMin value.
     *
     * @return the latitudeMin value.
     */
    public Double getLatitudeMin() {
        return latitudeMin;
    }

    /**
     * Sets the given value to latitudeMin.
     *
     * @param latitudeMin
     *            the given value to set.
     */
    public void setLatitudeMin(Double latitudeMin) {
        this.latitudeMin = latitudeMin;
    }

    /**
     * Gets the latitudeMax value.
     *
     * @return the latitudeMax value.
     */
    public Double getLatitudeMax() {
        return latitudeMax;
    }

    /**
     * Sets the given value to latitudeMax.
     *
     * @param latitudeMax
     *            the given value to set.
     */
    public void setLatitudeMax(Double latitudeMax) {
        this.latitudeMax = latitudeMax;
    }

    /**
     * Gets the illuminationMin value.
     *
     * @return the illuminationMin value.
     */
    public Double getIlluminationMin() {
        return illuminationMin;
    }

    /**
     * Sets the given value to illuminationMin.
     *
     * @param illuminationMin
     *            the given value to set.
     */
    public void setIlluminationMin(Double illuminationMin) {
        this.illuminationMin = illuminationMin;
    }

    /**
     * Gets the illuminationMax value.
     *
     * @return the illuminationMax value.
     */
    public Double getIlluminationMax() {
        return illuminationMax;
    }

    /**
     * Sets the given value to illuminationMax.
     *
     * @param illuminationMax
     *            the given value to set.
     */
    public void setIlluminationMax(Double illuminationMax) {
        this.illuminationMax = illuminationMax;
    }

    /**
     * Gets the cameraAngleMin value.
     *
     * @return the cameraAngleMin value.
     */
    public Double getCameraAngleMin() {
        return cameraAngleMin;
    }

    /**
     * Sets the given value to cameraAngleMin.
     *
     * @param cameraAngleMin
     *            the given value to set.
     */
    public void setCameraAngleMin(Double cameraAngleMin) {
        this.cameraAngleMin = cameraAngleMin;
    }

    /**
     * Gets the cameraAngleMax value.
     *
     * @return the cameraAngleMax value.
     */
    public Double getCameraAngleMax() {
        return cameraAngleMax;
    }

    /**
     * Sets the given value to cameraAngleMax.
     *
     * @param cameraAngleMax
     *            the given value to set.
     */
    public void setCameraAngleMax(Double cameraAngleMax) {
        this.cameraAngleMax = cameraAngleMax;
    }
    
    public String getProductType() {
        if (productType == null) {
            return null;
        }
        
        return productType.toUpperCase();
    }
    
    public void setProductType(String productType) {
        this.productType = productType;
    }
    
    public String getCameraSpecification() {
        if (cameraSpecification == null) {
            return null;
        }
        
        return cameraSpecification.toUpperCase();
    }
    
    public void setCameraSpecification(String cameraType) {
        this.cameraSpecification = cameraType;
    }

    /**
     * Gets the JSONObject instance.
     *
     * @return the JSONObject for this instance.
     */
    public JSONObject toJSONObject() {
        JSONObject object = new JSONObject();
        EntityHelper.setLong(object, "dataSetId", dataSetId);
        EntityHelper.setArray(object, "targetTypes", targetTypes);
        EntityHelper.setArray(object, "targets", targets);
        EntityHelper.setArray(object, "missions", missions);
        EntityHelper.setArray(object, "instruments", instruments);
        EntityHelper.setArray(object, "instrumentHosts", instrumentHosts);

        EntityHelper.setString(object, "startDate", EntityHelper.convertDateToString(startDate));
        EntityHelper.setString(object, "stopDate", EntityHelper.convertDateToString(stopDate));

        /*
        EntityHelper.setBoolean(object, "whiteSpots", whiteSpots);
        EntityHelper.setFloat(object, "lowerDataRating", lowerDataRating);
        EntityHelper.setFloat(object, "upperDataRating", upperDataRating);
        */

        EntityHelper.setBoolean(object, "useLRO", useLRO);
        EntityHelper.setDouble(object, "longitudeMin", longitudeMin);
        EntityHelper.setDouble(object, "longitudeMax", longitudeMax);
        EntityHelper.setDouble(object, "latitudeMin", latitudeMin);
        EntityHelper.setDouble(object, "latitudeMax", latitudeMax);
        EntityHelper.setDouble(object, "illuminationMin", illuminationMin);
        EntityHelper.setDouble(object, "illuminationMax", illuminationMax);
        EntityHelper.setDouble(object, "cameraAngleMin", cameraAngleMin);
        EntityHelper.setDouble(object, "cameraAngleMax", cameraAngleMax);
        return object;
    }

    /**
     * Gets the JSON string that represents this object.
     *
     * @return the JSON string that represents this object.
     */
    public String toJSONString() {
        return toJSONObject().toJSONString();
    }
}
