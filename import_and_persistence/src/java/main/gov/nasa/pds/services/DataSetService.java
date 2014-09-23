/*
 * Copyright (C) 2011 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.services;

import gov.nasa.pds.entities.DataFile;
import gov.nasa.pds.entities.DataSet;
import gov.nasa.pds.entities.EntityInfo;
import gov.nasa.pds.entities.Instrument;
import gov.nasa.pds.entities.MapImage;
import gov.nasa.pds.entities.Mission;
import gov.nasa.pds.entities.Page;
import gov.nasa.pds.entities.PagedResults;
import gov.nasa.pds.entities.Restriction;
import gov.nasa.pds.entities.SearchCriteria;
import gov.nasa.pds.entities.SearchResults;
import gov.nasa.pds.entities.Target;
import gov.nasa.pds.entities.TargetType;

import java.util.List;

/**
 * A comprehensive service to get data for the users.
 *
 * Thread Safety: The implementations should be effectively thread-safe.
 * 
 * <p>
 * Version 1.1 changes [NASA PDS API - LROC Update]:
 * <ol>
 * <li>Added {@link #searchMapImagesByCriteria(SearchCriteria, Page)}</li>
 * </ol>
 * </p>
 *
 * @author anon, schmoel
 * @version 1.1
 */
public interface DataSetService {
    /* ******************************************************************
     * Target Type operations *****************************************************************
     */
    PagedResults<EntityInfo> getTargetTypesInfo(Page page) throws DataSetProcessingException;

    TargetType getTargetType(long targetTypeId) throws DataSetProcessingException;

    EntitiesStats getTargetTypeChildrenStats(long targetTypeId) throws DataSetProcessingException;

    /* ******************************************************************
     * Target operations *****************************************************************
     */
    PagedResults<EntityInfo> getTargetsInfo(Page page, Restriction restriction) throws DataSetProcessingException;

    Target getTarget(long targetId) throws DataSetProcessingException;

    EntitiesStats getTargetChildrenStats(long targetId) throws DataSetProcessingException;

    /* ******************************************************************
     * Mission operations *****************************************************************
     */
    PagedResults<EntityInfo> getMissionsInfo(Page page, Restriction restriction) throws DataSetProcessingException;

    Mission getMission(long missionId) throws DataSetProcessingException;

    EntitiesStats getMissionChildrenStats(long missionId) throws DataSetProcessingException;

    /* ******************************************************************
     * Instrument operations *****************************************************************
     */
    PagedResults<EntityInfo> getInstrumentsInfo(Page page, Restriction restriction) throws DataSetProcessingException;

    Instrument getInstrument(long instrumentId) throws DataSetProcessingException;

    EntitiesStats getInstrumentChildrenStats(long instrumentId) throws DataSetProcessingException;

    /* ******************************************************************
     * DataSet operations *****************************************************************
     */
    PagedResults<EntityInfo> getDataSetsInfo(Page page, Restriction restriction) throws DataSetProcessingException;

    DataSet getDataSet(long dataSetId) throws DataSetProcessingException;

    EntitiesStats getDataSetChildrenStats(long dataSetId) throws DataSetProcessingException;

    List<EntityInfo> getDataSetRelatedEntitites(long dataSetId, Class<?> entityClass) throws DataSetProcessingException;

    /* ******************************************************************
     * DataFile operations *****************************************************************
     */
    PagedResults<EntityInfo> getDataFilesInfo(Page page, Restriction restriction) throws DataSetProcessingException;

    PagedResults<EntityInfo> getImagesInfo(Page page, Restriction restriction) throws DataSetProcessingException;

    DataFile getDataFile(long dataFileId) throws DataSetProcessingException;

    String getPreviewImageURL(long imageFileId) throws DataSetProcessingException;

    /* **************************************************************************************
     * Search support *************************************************************************************
     */
    SearchResults searchEntities(String searchText, Page page) throws DataSetProcessingException;

    PagedResults<EntityInfo> searchEntitiesByType(Class<?> entityClass, String searchText, Page page,
            Restriction restriction) throws DataSetProcessingException;

    PagedResults<EntityInfo> searchDataSetsByCriteria(SearchCriteria criteria, Page page)
            throws DataSetProcessingException;

    /**
     * Allows callers to retrieve a page of EntityInfo's pertaining to {@link MapImage} entities, for those MapImages
     * that meet the search criteria
     * 
     * @param criteria
     *            the criteria to meet
     * @param page
     *            the page to pull
     * @return the paged results
     * @throws DataSetProcessingException
     */
    PagedResults<EntityInfo> searchMapImagesByCriteria(SearchCriteria criteria, Page page)
            throws DataSetProcessingException;

    /**
     * Returns the paths to the given {@link MapImage}s
     * @param mapImageEntityInfos the {@link EntityInfo}s representing our MapImages
     * @return the paths
     */
    List<String> getMapImagePaths(List<EntityInfo> mapImageEntityInfos);
    
    /**
     * @return all the product types in the database.
     */
    List<String> getAllProductTypes();
    
    /**
     * @return all the camera specs in the database.
     */
    List<String> getAllCameraSpecs();
}
