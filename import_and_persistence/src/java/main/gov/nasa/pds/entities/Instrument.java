/*
 * Copyright (C) 2011 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.entities;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.topcoder.json.object.JSONObject;

/**
 * This class represents an instrument.
 *
 * Thread Safety: This class is mutable and not thread safe.
 */
public class Instrument extends NamedEntity implements MetaDataObjectConsumer {
    /**
     * Constant for "INSTRUMENT_ID" String.
     */
    private static final String INSTRUMENT_ID = "INSTRUMENT_ID";
    /**
     * Constant for "INSTRUMENT_INFORMATION" String.
     */
    private static final String INSTRUMENT_INFORMATION = "INSTRUMENT_INFORMATION";
    /**
     * Constant for "INSTRUMENT_NAME" String.
     */
    private static final String INSTRUMENT_NAME = "INSTRUMENT_NAME";
    /**
     * Constant for "INSTRUMENT_HOST" String.
     */
    private static final String INSTRUMENT_HOST_ID = "INSTRUMENT_HOST_ID";
    /**
     * Constant for "INSTRUMENT_TYPE" String.
     */
    private static final String INSTRUMENT_TYPE = "INSTRUMENT_TYPE";
    /**
     * Constant for "INSTRUMENT_DESC" String.
     */
    private static final String INSTRUMENT_DESC = "INSTRUMENT_DESC";
    /**
     * Constant for "INSTRUMENT_REFERENCE_INFO" String.
     */
    private static final String INSTRUMENT_REFERENCE_INFO = "INSTRUMENT_REFERENCE_INFO";

    /**
     * Represents the primary ID defined in the original file. It is managed with a getter and setter. It may have any
     * value. It is fully mutable.
     */
    private String textId;
    /**
     * Represents the hosts. It is managed with a getter and setter. It may have any value. It is fully mutable.
     */
    private List<InstrumentHost> hosts;
    /**
     * Represents the type. It is managed with a getter and setter. It may have any value. It is fully mutable.
     */
    private String type;
    /**
     * Represents the description. It is managed with a getter and setter. It may have any value. It is fully mutable.
     */
    private String description;
    /**
     * Represents the references. It is managed with a getter and setter. It may have any value. It is fully mutable.
     */
    private List<Reference> references;
    /**
     * Represents the structure of other objects of this entity. It is managed with a getter and setter. It may have any
     * value. It is fully mutable.
     */
    private List<MetadataObject> otherChildren;

    /**
     * Empty constructor.
     */
    public Instrument() {
    }

    /**
     * Constructor. Create the instance with name.
     *
     * @param name
     *            - the given name to set.
     */
    public Instrument(String name) {
        super(name);
    }

    /**
     * Populates the instance with given metaDataObject.
     *
     * @param metaDataObject
     *            - the given object to handle.
     */
    @Override
    public void fromMetadata(MetadataObject metaDataObject) {
        if (metaDataObject == null) {
            return;
        }

        List<Property> properties = metaDataObject.getProperties();
        List<MetadataObject> children = metaDataObject.getChildren();
        Map<String, Property> map = EntityHelper.getProperties(properties, new String[] { INSTRUMENT_ID, INSTRUMENT_HOST_ID });

        this.textId = EntityHelper.getPropertyStringValue(map.get(INSTRUMENT_ID));

        hosts = new ArrayList<InstrumentHost>();
        InstrumentHost instrumentHost = new InstrumentHost();
        instrumentHost.setTextId(EntityHelper.getPropertyStringValue(map.get(INSTRUMENT_HOST_ID)));
        hosts.add(instrumentHost);

        references = EntityHelper.createReferencesList(INSTRUMENT_REFERENCE_INFO, children);
        // read the data from information section
        MetadataObject instrumentInfo = EntityHelper.findMetadataObject(INSTRUMENT_INFORMATION, children);
        if (instrumentInfo != null) {
            Map<String, Property> instrumentInfoMap = EntityHelper.getProperties(instrumentInfo.getProperties(),
                    new String[] { INSTRUMENT_NAME, INSTRUMENT_TYPE, INSTRUMENT_DESC });
            setName(EntityHelper.getPropertyStringValue(instrumentInfoMap.get(INSTRUMENT_NAME)));
            this.type = EntityHelper.getPropertyStringValue(instrumentInfoMap.get(INSTRUMENT_TYPE));
            this.description = EntityHelper.getPropertyStringValue(instrumentInfoMap.get(INSTRUMENT_DESC));
        }

        this.otherChildren = children;
    }

    /**
     * Gets the textId value.
     *
     * @return - the textId value.
     */
    public String getTextId() {
        return textId;
    }

    /**
     * Sets the given value to textId.
     *
     * @param textId
     *            - the given value to set.
     */
    public void setTextId(String textId) {
        this.textId = textId;
    }

    /**
     * Gets the hosts value.
     *
     * @return - the hosts value.
     */
    public List<InstrumentHost> getHosts() {
        return hosts;
    }

    /**
     * Sets the given value to hosts.
     *
     * @param hosts
     *            - the given value to set.
     */
    public void setHosts(List<InstrumentHost> hosts) {
        this.hosts = hosts;
    }

    /**
     * Gets the type value.
     *
     * @return - the type value.
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the given value to type.
     *
     * @param type
     *            - the given value to set.
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Gets the description value.
     *
     * @return - the description value.
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the given value to description.
     *
     * @param description
     *            - the given value to set.
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Gets the references value.
     *
     * @return - the references value.
     */
    public List<Reference> getReferences() {
        return references;
    }

    /**
     * Sets the given value to references.
     *
     * @param references
     *            - the given value to set.
     */
    public void setReferences(List<Reference> references) {
        this.references = references;
    }

    /**
     * Gets the otherChildren value.
     *
     * @return - the otherChildren value.
     */
    public List<MetadataObject> getOtherChildren() {
        return otherChildren;
    }

    /**
     * Sets the given value to otherChildren.
     *
     * @param otherChildren
     *            - the given value to set.
     */
    public void setOtherChildren(List<MetadataObject> otherChildren) {
        this.otherChildren = otherChildren;
    }

    /**
     * Gets the JSONObject instance.
     *
     * @return - the JSONObject for this instance.
     */
    @Override
    public JSONObject toJSONObject() {
        JSONObject object = super.toJSONObject();
        EntityHelper.setString(object, "textId", textId);
        EntityHelper.setArray(object, "hosts", hosts);
        EntityHelper.setString(object, "type", type);
        EntityHelper.setString(object, "description", description);
        EntityHelper.setArray(object, "references", references);
        EntityHelper.setArray(object, "otherChildren", otherChildren);
        return object;
    }
}
