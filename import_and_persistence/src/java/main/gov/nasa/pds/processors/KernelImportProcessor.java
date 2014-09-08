/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.processors;

import gov.nasa.pds.services.DataSetProcessingException;

/**
 * <p>
 * The service is responsible for importing kernel programs.
 * </p>
 *
 * <strong>Thread Safety:</strong> The implementations are not required to be thread-safe.
 *
 * @author fivestarwy, caoweiquan322
 * @version 1.0
 */
public interface KernelImportProcessor {
    /**
     * Retrieve all Core kernel programs and LRO-specific kernels (LRO-specific kernels and store the kernels
     * plainly in a file system in some configurable folder
     *
     * @throws DataSetProcessingException
     *             if there is any error while importing kernels.
     */
    void importKernels() throws DataSetProcessingException;
}

