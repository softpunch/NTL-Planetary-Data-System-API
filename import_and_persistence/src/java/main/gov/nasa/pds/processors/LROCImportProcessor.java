/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.processors;

import gov.nasa.pds.services.DataSetProcessingException;

/**
 * <p>
 * The service is responsible for importing images from camera.
 * </p>
 *
 * <strong>Thread Safety:</strong> The implementation should be thread safe.
 *
 * <p>
 * Version 1.1 changes [NASA PDS API - LROC Update]:
 * <ol>
 * <li>added productType and args param to {@link #importMapData(String, String)}</li>
 * </ol>
 * </p>
 * 
 * @author fivestarwy, caoweiquan322, schmoel
 * @version 1.1
 */
public interface LROCImportProcessor {
    /**
     * Import map data.
     * 
     * @param productType
     *            the product type to import for.
     * @param args
     *
     * @throws DataSetProcessingException
     *             if there is any error while importing LROC data
     */
    void importMapData(String productType, String args) throws DataSetProcessingException;
}
