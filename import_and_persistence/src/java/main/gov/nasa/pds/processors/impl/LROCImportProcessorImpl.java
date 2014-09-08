/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.processors.impl;

import gov.nasa.pds.entities.CameraSpecification;
import gov.nasa.pds.entities.CameraType;
import gov.nasa.pds.entities.MapImage;
import gov.nasa.pds.entities.Mission;
import gov.nasa.pds.entities.ProductType;
import gov.nasa.pds.processors.LROCImportProcessor;
import gov.nasa.pds.services.ConversionPersistence;
import gov.nasa.pds.services.ConversionPersistence.LrocIds;
import gov.nasa.pds.services.DataSetProcessingConfigurationException;
import gov.nasa.pds.services.DataSetProcessingException;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.validator.routines.UrlValidator;
import org.springframework.beans.factory.InitializingBean;

import com.topcoder.commons.utils.LoggingWrapperUtility;
import com.topcoder.commons.utils.ValidationUtility;
import com.topcoder.json.object.JSONArray;
import com.topcoder.json.object.JSONDataAccessTypeException;
import com.topcoder.json.object.JSONInvalidKeyException;
import com.topcoder.json.object.JSONObject;
import com.topcoder.json.object.io.JSONDecoder;
import com.topcoder.json.object.io.JSONDecodingException;
import com.topcoder.json.object.io.StandardJSONDecoder;
import com.topcoder.util.log.Level;
import com.topcoder.util.log.Log;

/**
 * <p>
 * The {@link LROCImportProcessorImpl} class implements {@link LROCImportProcessor} interface by
 * providing implementation of <code>importMapData()</code> method.
 * </p>
 * <p>
 * This class will retrieves all collected images using web service and store them in local
 * persistence as separate data set. In addition, the image contents should be formatted to GeoTiff
 * format using gdal_translate utility. The image will be stored in additional model
 * {@link MapImage}.
 * </p>
 *
 * <strong>Thread Safety:</strong> The implementation should be thread safe.
 *
 * <b>Version 1.1</b>
 * <ul>
 * <li>Set MapImage's productType field in {@link #fetchMapImage(JSONObject, Mission, long)}</li>
 * <li>Removed parseMission() method. It was not inserting the correct data. We assume the LROC
 * volumes' documents have already been loaded instead and search for the mission directly</li>
 * </ul>
 * 
 * <b>Version 1.2</b>
 * <ul>
 * <li>Removed Mission parsing logic - we rely on this data having been previously imported</li>
 * <li>Removed geotiff logic - we download the IMG from the LROC source system directly</li>
 * </ul>
 *
 * <b>Version 1.3</b>
 * <ul>
 * <li>Removed image download logic. Store image url in MapImage.imagePath</li>
 * </ul>
 *
 * @author fivestarwy, caoweiquan322, schmoel
 * @version 1.3
 */
public class LROCImportProcessorImpl implements LROCImportProcessor, InitializingBean {
    /**
     * Constant for the class name of this class. Used for logging.
     */
    private static final String CLASS_NAME = LROCImportProcessorImpl.class.getName();

    /**
     * Constant for the PNG file suffix.
     */
    private static final String PNG_SUFFIX = ".png";

    /**
     * Constant for the TIF file suffix.
     */
    private static final String TIF_SUFFIX = ".tif";

    /**
     * Constant for the namesake string.
     */
    private static final String PRODUCT_ID_EQUALS = "product_id=";

    /**
     * Constant for the namesake string.
     */
    private static final String LIMIT_EQUALS = "limit=";

    /**
     * The maximum number of images to retrieve once.
     */
    private static final int MAXIMUM_IMAGES_RETRIEVED_ONCE = 100;

    /**
     * Constant for the default date format string.
     */
    private static final String DEFAULT_DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS";

    /**
     * Constant map storing the default values for optional fields.
     */
    private static final Map<String, String> OPTIONAL_FIELDS_DEFAULT_VALUE;

    /**
     * Constant for those keys that would be excluded from the reference list.
     */
    private static final List<String> KEYS_EXCLUDED_FROM_REFERENCES_LIST;

    /**
     * Initializes {@code OPTIONAL_FIELDS_DEFAULT_VALUE} and
     * {@code KEYS_EXCLUDED_FROM_REFERENCES_LIST}.
     */
    static {
        // Initialize field OPTIONAL_FIELDS_DEFAULT_VALUE.
        // As confirmed by PM, all default value should be set as null,
        // and then skip the record if any field is missing.
        Map<String, String> map = new HashMap<String, String>();
        // For Mission
        map.put("Observation_id", null);
        map.put("UTC_start_time", null);
        map.put("UTC_stop_time", null);
        map.put("Description", null);
        map.put("ProductURL", null);

        // For MapImage
        map.put("Observation_time", null);
        map.put("Center_longitude", null);
        map.put("Center_latitude", null);
        map.put("Solar_time", null);
        map.put("Phase_angle", null);

        OPTIONAL_FIELDS_DEFAULT_VALUE = Collections.unmodifiableMap(map);

        // Initialize field KEYS_EXCLUDED_FROM_REFERENCES_LIST.
        List<String> strList = new ArrayList<String>();
        strList.add("UTC_start_time");
        strList.add("UTC_stop_time");
        strList.add("Description");
        strList.add("Product_creation_time");
        strList.add("Target_name");
        strList.add("Product_version_id");
        strList.add("pdsid");
        strList.add("Observation_time");
        strList.add("Center_longitude");
        strList.add("Center_latitude");
        strList.add("Solar_time");
        strList.add("Phase_angle");
        KEYS_EXCLUDED_FROM_REFERENCES_LIST = Collections.unmodifiableList(strList);
    }

    /**
     * <p>
     * The <code>Log</code> instance used for logging.
     * </p>
     *
     * <p>
     * It is initialized with Spring setter dependency injection. Cannot be <code>null</code> after
     * initialization, assuming that property is initialized via Spring setter-based dependency
     * injection and is never changed after that. Has a setter.
     * </p>
     */
    private Log logger;

    /**
     * <p>
     * The <code>ConversionPersistence</code> instance. It will be use to persistence
     * {@link Mission} data and {@link MapImage} data.
     * </p>
     *
     * <p>
     * It is initialized with Spring setter dependency injection. Cannot be <code>null</code> after
     * initialization, assuming that property is initialized via Spring setter-based dependency
     * injection and is never changed after that. Has a setter.
     * </p>
     */
    private ConversionPersistence conversionPersistence;

    /**
     * <p>
     * The url to perform request to web service.
     * </p>
     *
     * <p>
     * It is initialized with Spring setter dependency injection. Cannot be <code>null</code> after
     * initialization, assuming that property is initialized via Spring setter-based dependency
     * injection and is never changed after that. Has a setter.
     * </p>
     */
    private String requestUrl;

    /**
     * Empty constructor.
     */
    public LROCImportProcessorImpl() {
    }

    /**
     * <p>
     * The method will retrieve all product metadata and image data from using web service and parse
     * the metadata to {@link Mission} object and persistent it. The image data will be formatted to
     * GeoTiff format using gdal_translate tool and saved to file system.
     * </p>
     * <p>
     * The method will send "Get" request to retrieve the product metadata via the web service with
     * JSON output URL like:
     * http://oderest.rsl.wustl.edu/live/?query=products&results=mb&limit=3&offset=0&target=Mars
     * &ihid=MRO&iid=HiRISE&pt=rdrv11&output=JSON this is a sample request with first 3 products of
     * target Mars returned.
     * </p>
     * <p>
     * The image data is kept in "Browser" field and it's a base64 string of PNG image.
     * </p>
     *
     * @throws DataSetProcessingException
     *             if there is any error while importing LROC data
     */
    @Override
    public void importMapData(String productType, String cameraType) throws DataSetProcessingException {
        final String signature = CLASS_NAME + ".importMapData()";
        LoggingWrapperUtility.logEntrance(logger, signature, null, null);

        // add product type to url
        String myRequestUrl = this.requestUrl + "&pt=" + productType;
        
        // Import map data
        try {
            // Invoke internal method.
            if (myRequestUrl.contains(LIMIT_EQUALS)) {
                importMapDataWithLimitOffset(myRequestUrl, cameraType);
            } else {
                int numCount = getProductsCount(myRequestUrl);
                int offset = 0;
                while (numCount > 0) {
                    importMapDataWithLimitOffset(myRequestUrl + "&offset=" + offset + "&limit="
                            + MAXIMUM_IMAGES_RETRIEVED_ONCE, cameraType);
                    offset += MAXIMUM_IMAGES_RETRIEVED_ONCE;
                    numCount -= MAXIMUM_IMAGES_RETRIEVED_ONCE;
                }
            }
        } catch (DataSetProcessingException e) {
            throw LoggingWrapperUtility.logException(logger, signature, e);
        }

        LoggingWrapperUtility.logExit(logger, signature, null);
    }

    /**
     * <p>
     * The internal helper method will retrieve product metadata and image data with specified
     * limited request URL.
     * </p>
     *
     * @param subRequestUrl
     *            the URL that has limit and offset parameter attached
     * @throws DataSetProcessingException
     *             if there is any error while importing LROC data
     */
    private void importMapDataWithLimitOffset(String subRequestUrl, String cameraType) throws DataSetProcessingException {
        String jsonStr = getJsonString(subRequestUrl);

        // Import map data
        try {
            // Parse the input stream to get the JSON data.
            JSONDecoder decoder = new StandardJSONDecoder();

            // HACK -- http://apps.topcoder.com/forums/?module=Thread&threadID=828138&start=0

            Pattern pattern = Pattern.compile("\"RelativePathtoVol\": \"([^\"]*)\"");
            Matcher matcher = pattern.matcher(jsonStr);

            List<String> relativePathsToVol = new ArrayList<String>();

            while (matcher.find()) {
                relativePathsToVol.add(matcher.group(1));
            }

            jsonStr = matcher.replaceAll("\"RelativePathtoVol\":\"\"");

            // HACK -- http://apps.topcoder.com/forums/?module=Thread&threadID=828138&start=0

            JSONObject decodedJSONObj = (JSONObject) decoder.decode(jsonStr);

            JSONObject productsJsonObject = decodedJSONObj.getNestedObject("ODEResults").getNestedObject("Products");
            JSONArray productsArray = null;

            try {
                // for each product, JSON path like: ODEResults ---> Products ---> Product:
                productsArray = productsJsonObject.getArray("Product");
            } catch (JSONDataAccessTypeException e) {
                productsArray = new JSONArray();
                productsArray.addJSONObject(productsJsonObject.getNestedObject("Product"));
            }

            for (int i = 0; i < productsArray.getSize(); i++) {
                JSONObject product = productsArray.getJSONObject(i);
                product.setString("RelativePathtoVol", relativePathsToVol.get(i));

                String dataSetTextId = product.getString("Data_Set_Id");

                LrocIds lrocIds = conversionPersistence.findLrocIds(dataSetTextId);

                // create MapImage entity from corresponding <key,value> pair
                MapImage mapImage = fetchMapImage(product, lrocIds.getMissionId(), cameraType);
                if (mapImage == null) {
                    continue;
                }
                conversionPersistence.insertMapImage(mapImage, lrocIds.getDataSetId());
            }
        } catch (DataSetProcessingException e) {
            throw e;
        } catch (JSONDecodingException e) {
            throw new DataSetProcessingException("Decoding JSON string error.", e);
        }
    }

    /**
     * <p>
     * The helper method to get the number of products that could be potentially retrieved from REST
     * service.
     * </p>
     *
     * @param strUrl
     *            the REST service URL
     * @return number of potential products for the specified URL
     */
    private static int getProductsCount(String strUrl) {
        int numProducts = 0;
        try {
            strUrl = strUrl.replaceAll("results=[a-z]+", "results=c");
            String jsonStr = getJsonString(strUrl);
            // Parse the input stream to get the JSON data.
            JSONDecoder decoder = new StandardJSONDecoder();
            JSONObject decodedJSONObj = (JSONObject) decoder.decode(jsonStr);
            numProducts = Integer.parseInt(decodedJSONObj.getNestedObject("ODEResults").getString("Count"));
        } catch (Exception e) {
            // Yield
        }

        return numProducts;
    }

    /**
     * <p>
     * Get the JSON string returned from the specified URL.
     * </p>
     *
     * @param strUrl
     *            the URL that to query results
     * @return the JSON string returned from the specified URL
     * @throws DataSetProcessingException
     *             if there is any error while retrieving query results
     */
    private static String getJsonString(String strUrl) throws DataSetProcessingException {
        // Get the input stream.
        InputStream raw = null;
        HttpURLConnection uc = null;
        String jsonStr = null;
        try {
            URL urlObj = new URL(strUrl);
            uc = (HttpURLConnection) urlObj.openConnection();
            raw = uc.getInputStream();
            jsonStr = Helper.readAllFromStream(raw);
        } catch (IOException e) {
            throw new DataSetProcessingException("There is error while connecting to URL '" + strUrl + "'", e);
        } catch (ClassCastException e) {
            throw new DataSetProcessingException("There is error while converting connection to http type", e);
        } catch (SecurityException e) {
            throw new DataSetProcessingException("There is security error while connecting to URL", e);
        } finally {
            Helper.closeIO(raw);
            if (uc != null) {
                try {
                    uc.disconnect();
                } catch (Exception e2) {
                    // Yield
                }
            }
        }

        return jsonStr;
    }

    /**
     * Method for parsing {@link MapImage} from the {@link JSONObject}.
     *
     * @param product
     *            the JSON object to parse {@link MapImage}
     * @param mission
     *            the {@link Mission} assigned to this {@link MapImage}
     * @param missionId
     *            the ID of mission
     * @param cameraType 
     * @return the {@link MapImage} object
     * @throws DataSetProcessingException
     *             if there is any error if there is any error while parsing the image
     */
    private MapImage fetchMapImage(JSONObject product, long missionId, String cameraType) throws DataSetProcessingException {
        try {
            SimpleDateFormat simpleDateFormat = new SimpleDateFormat(DEFAULT_DATE_FORMAT);

            MapImage mapImage = new MapImage();
            mapImage.setProductType(parseProductType(product));
            mapImage.setName(parseMapImageFileName(product));
            mapImage.setMissionId(missionId);
            mapImage.setImagePath(product.getString("LabelURL"));
            mapImage.setCameraSpecification(CameraSpecification.valueOf(cameraType.toUpperCase()));
            String observationTime = getOptionalField(product, "Observation_time");
            if (isNotNullNorEmpty(observationTime)) {
                mapImage.setDate(simpleDateFormat.parse(observationTime));
            } else {
                mapImage.setDate(null);
            }
            String centerLongitude = getOptionalField(product, "Center_longitude");
            if (isNotNullNorEmpty(centerLongitude)) {
                mapImage.setCenterLongitude(Double.valueOf(centerLongitude));
            } else {
                mapImage.setCenterLongitude(null);
            }
            String centerLatitude = getOptionalField(product, "Center_latitude");
            if (isNotNullNorEmpty(centerLatitude)) {
                mapImage.setCenterLatitude(Double.valueOf(centerLatitude));
            } else {
                mapImage.setCenterLatitude(null);
            }
            String solarTime = getOptionalField(product, "Solar_time");
            if (isNotNullNorEmpty(solarTime)) {
                mapImage.setIllumination(Double.valueOf(solarTime));
            } else {
                mapImage.setIllumination(null);
            }
            String phaseAngle = getOptionalField(product, "Phase_angle");
            if (isNotNullNorEmpty(phaseAngle)) {
                mapImage.setCameraAngle(Double.valueOf(phaseAngle));
                if (mapImage.getCameraAngle() > 90) {
                    mapImage.setCameraType(CameraType.LROC_LEFT);
                } else {
                    mapImage.setCameraType(CameraType.LROC_RIGHT);
                }
            } else {
                mapImage.setCameraAngle(null);
                mapImage.setCameraType(CameraType.LROC_UNKNOWN);
            }

            return mapImage;
        } catch (ParseException e) {
            throw new DataSetProcessingException("Parsing date error.", e);
        } catch (NullPointerException e) {
            // Some require field is null, skip this record by returning null pointer.
            return null;
        } catch (NumberFormatException e) {
            throw new DataSetProcessingException("The number field is of wrong format.", e);
        }
    }

    /**
     * Fetches the product type from the product JSON
     *
     * @param product
     *            the product to pull the product type form
     * @return The ProductType
     * @throw IllegalStateException when we the product type is unknown to us
     */
    private static ProductType parseProductType(JSONObject product) {
        String pt = getOptionalField(product, "pt");

        try {
            return ProductType.valueOf(pt);
        } catch (IllegalArgumentException e) {
            throw new IllegalStateException("The pt in the response was unknown to us: " + pt, e);
        }
    }

    /**
     * Method for parsing map image file name from the {@link JSONObject}.
     *
     * @param product
     *            the JSON object to parse {@link MapImage} file name
     * @return the file name of the {@link MapImage}
     * @throws NullPointerException
     *             if there is any error if there's no legal file name to return
     */
    private static String parseMapImageFileName(JSONObject product) {
        String productURL = getOptionalField(product, "ProductURL");
        String productIdKey = PRODUCT_ID_EQUALS;
        int productIdKeyIndex = productURL.indexOf(productIdKey);
        if (productIdKeyIndex < 0) {
            // Throw NullPointerException to caller, so that this product could be skipped.
            throw new NullPointerException("The required file name is missing from ProductURL.");
        }
        int productIdValueIndex = productIdKeyIndex + productIdKey.length();
        int productIdValueEnd = productURL.indexOf("&", productIdValueIndex);
        if (productIdValueEnd < 0) {
            productIdValueEnd = productURL.length();
        }
        String fileName = productURL.substring(productIdValueIndex, productIdValueEnd);
        if (fileName.isEmpty()) {
            // Throw NullPointerException to caller, so that this product could be skipped.
            throw new NullPointerException("The required file name could not be empty.");
        }

        return fileName;
    }

    /**
     * Method for getting optional field from a {@link JSONObject}. If key of an optional field is
     * missing, then the default value will be returned.
     *
     * @param product
     *            the JSON object to retrieve value
     * @param key
     *            key of the field
     * @return the corresponding value of the key
     */
    private static String getOptionalField(JSONObject product, String key) {
        String value = null;
        try {
            value = product.getString(key);
        } catch (JSONInvalidKeyException e) {
            // Yield
        }

        if (value == null) {
            value = OPTIONAL_FIELDS_DEFAULT_VALUE.get(key);
        }
        return value;
    }

    /**
     * Method for checking if a string is not null nor empty.
     *
     * @param str
     *            the string to check
     * @return true if str is not null nor empty, false otherwise
     */
    private static boolean isNotNullNorEmpty(String str) {
        return (str != null && !str.isEmpty());
    }

    /**
     * <p>
     * Check whether this class was initialized by spring properly.
     * </p>
     * <p>
     * Required parameters:
     * <ul>
     * <li>logger</li>
     * <li>conversionPersistence</li>
     * <li>requestUrl</li>
     * </ul>
     * </p>
     *
     * @throws DataSetProcessingConfigurationException
     *             if logger, conversionPersitence is null or requestUrl is null/empty or invalid
     *             URL.
     */
    @Override
    public void afterPropertiesSet() {
        ValidationUtility.checkNotNull(logger, "logger", DataSetProcessingConfigurationException.class);
        ValidationUtility.checkNotNull(conversionPersistence, "conversionPersistence",
                DataSetProcessingConfigurationException.class);
        ValidationUtility.checkNotNullNorEmptyAfterTrimming(requestUrl, "requestUrl",
                DataSetProcessingConfigurationException.class);

        // Check if requestUrl is a valid URL.
        // Currently ODE REST service only support http.
        String[] schemes = { "http" };
        UrlValidator urlValidator = new UrlValidator(schemes);
        if (!urlValidator.isValid(requestUrl)) {
            throw new DataSetProcessingConfigurationException("requestUrl is an invalid http URL.");
        }
    }

    /**
     * Sets the logger.
     *
     * @param logger
     *            the logger to set
     */
    public void setLogger(Log logger) {
        this.logger = logger;
    }

    /**
     * Sets the conversionPersistence.
     *
     * @param conversionPersistence
     *            the conversionPersistence to set
     */
    public void setConversionPersistence(ConversionPersistence conversionPersistence) {
        this.conversionPersistence = conversionPersistence;
    }

    /**
     * Sets the request URL.
     *
     * @param requestUrl
     *            the request URL to set
     */
    public void setRequestUrl(String requestUrl) {
        this.requestUrl = requestUrl;
    }
}
