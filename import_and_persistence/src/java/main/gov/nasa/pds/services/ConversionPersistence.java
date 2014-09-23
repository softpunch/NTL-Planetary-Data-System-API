/*
 * Copyright (C) 2011-2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.services;

import gov.nasa.pds.entities.DataFile;
import gov.nasa.pds.entities.DataSet;
import gov.nasa.pds.entities.Instrument;
import gov.nasa.pds.entities.InstrumentHost;
import gov.nasa.pds.entities.MapImage;
import gov.nasa.pds.entities.Mission;
import gov.nasa.pds.entities.Product;
import gov.nasa.pds.entities.Reference;
import gov.nasa.pds.entities.Table;
import gov.nasa.pds.entities.Target;
import gov.nasa.pds.entities.Volume;
import gov.nasa.pds.processors.DataSetProcessor;
import gov.nasa.pds.processors.impl.profile.cassini.CassiniObservationInfo;

import java.util.List;

/**
 * The persistence service used during the conversion process. Used to insert converted data.
 *
 * Thread Safety: The implementations should be effectively thread-safe.
 *
 * <p>
 * Version 1.1 changes [PDS - Import and Persistence Update - Assembly Contest]:
 * <ol>
 * <li>Added {@link #insertCassiniObservationInfo(CassiniObservationInfo)} method.</li>
 * </ol>
 * </p>
 * <p>
 * Version 1.3 changes [NASA LMMP - PDS API Update Module Assembly]:
 * <ol>
 * <li>Added {@link #insertMapImage(MapImage)} method.</li>
 * </ol>
 * </p>
 *
 * <p>
 * Version 1.4 changes [NASA PDS API - LROC Update]:
 * <ol>
 * <li>Added {@link LrocIds} type</li>
 * <li>Added {@link #findLrocMission(String)} method.</li>
 * <li>Added {@link #insertMapImage(MapImage, long)}</li>
 * </ol>
 * </p>
 *
 * @author TCSASSEMBLER, fivestarwy, caoweiquan322, schmoel
 * @version 1.5
 */
public interface ConversionPersistence {
    /**
     * Encapsulates some identifiers that are of interest to the LROC image importer.
     */
    public static class LrocIds {
        /** the data set id */
        private final long dataSetId;

        /** the data set's LRO mission id */
        private final long missionId;

        public LrocIds(long dataSetId, long missionId) {
            this.dataSetId = dataSetId;
            this.missionId = missionId;
        }

        public long getDataSetId() {
            return dataSetId;
        }

        public long getMissionId() {
            return missionId;
        }
    }

    /**
     * Information about newly created table.
     */
    public static class TableInfo {
        /**
         * The SQL table name.
         */
        private final String sqlTableName;
        /**
         * The database table identifier.
         */
        private final long tableId;

        /**
         * Creates an instance of {@code TableInfo}.
         */
        public TableInfo(String sqlTableName, long tableId) {
            this.sqlTableName = sqlTableName;
            this.tableId = tableId;
        }

        /**
         * Gets the SQL table name.
         */
        public String getSQLTableName() {
            return sqlTableName;
        }

        /**
         * Gets the database table identifier.
         */
        public long getTableId() {
            return tableId;
        }
    }

    /**
     * Writes cached data to DB from insert caches. Clears all the caches (insert caches and caches
     * used to speed up DB operations).
     */
    void clearCaches();

    /**
     * Creates the table with the given columns. All columns will be of 'varchar' type of size that
     * is determined by the columnSizes parameter.
     *
     * @param columnNames
     *            the column names
     * @param columnSizes
     *            the column sizes
     *
     * @return the information about newly created table
     *
     * @throws DataSetProcessingException
     *             if failed to create the table
     */
    TableInfo createTable(List<String> columnNames, List<Integer> columnSizes) throws DataSetProcessingException;

    /**
     * Drops a table with a given name.
     *
     * @param tableName
     *            the name of the table to drop
     *
     * @throws DataSetProcessingException
     *             if failed to drop the table
     */
    void dropTable(String tableName) throws DataSetProcessingException;

    /**
     * Inserts the given data into the given table into persistence.
     *
     * @param tableId
     *            defines the table for data inserting
     * @param table
     *            the table instance populated with data to insert
     *
     * @return the new data table id
     *
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data
     */
    long insertDataIntoTable(long tableId, Table table) throws DataSetProcessingException;

    /**
     * Inserts data into the database table specified by table#sqlTableName property.
     *
     * @param table
     *            the table which contains data to insert into the database
     * @throws DataSetProcessingException
     *             if error occurs while persisting the data
     */
    void insertDataIntoRegularTable(Table table) throws DataSetProcessingException;

    /**
     * Inserts the given object into persistence.
     *
     * @param volume
     *            the given volume instance.
     *
     * @return the id of volume.
     *
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data
     */
    long insertVolume(Volume volume) throws DataSetProcessingException;

    /**
     * Inserts the given object into persistence.
     *
     * @param reference
     *            the given reference instance.
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data
     * @return the id of reference.
     */
    long insertReference(Reference reference) throws DataSetProcessingException;

    /**
     * Inserts the given object into persistence.
     *
     * @param instrumentHost
     *            the given instrumentHost instance.
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data
     * @return the id of instrumentHost
     */
    long insertInstrumentHost(InstrumentHost instrumentHost) throws DataSetProcessingException;

    /**
     * Inserts the given object into persistence.
     *
     * @param instrument
     *            the given instrument instance.
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data
     * @return the id of instrument.
     */
    long insertInstrument(Instrument instrument) throws DataSetProcessingException;

    /**
     * Inserts the given object into persistence.
     *
     * @param mission
     *            the given mission instance.
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data
     * @return the id of mission
     */
    long insertMission(Mission mission) throws DataSetProcessingException;

    /**
     * Inserts the given object into persistence.
     *
     * @param target
     *            the given target instance.
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data
     * @return the id of target
     */
    long insertTarget(Target target) throws DataSetProcessingException;

    /**
     * Inserts the given object into persistence.
     *
     * @param dataSet
     *            the given dataSet instance.
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data
     * @return the id of dataSet.
     */
    long insertDataSet(DataSet dataSet) throws DataSetProcessingException;

    /**
     * Inserts the given object into persistence.
     *
     * @param product
     *            the given product instance.
     * @param dataSetId
     *            the given dataSetId value.
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data.
     * @return the id of product.
     */
    long insertProduct(long dataSetId, Product product) throws DataSetProcessingException;

    /**
     * Inserts the given object into persistence.
     *
     * @param dataFile
     *            the given dataFile instance.
     * @param productId
     *            the given productId value.
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data.
     */
    void insertProductDocument(long productId, DataFile dataFile) throws DataSetProcessingException;

    /**
     * Inserts the given object into persistence.
     *
     * @param dataSetId
     *            the given dataSetId value.
     * @param dataFile
     *            the given dataFile instance.
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data.
     * @return - the id of dataFile
     */
    long insertDataSetDocument(long dataSetId, DataFile dataFile) throws DataSetProcessingException;

    /**
     * Inserts Cassini observation information.
     *
     * @param observationInfo
     *            the observation data
     *
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data
     *
     * @since 1.2
     */
    void insertCassiniObservationInfo(CassiniObservationInfo observationInfo) throws DataSetProcessingException;

    /**
     * Associates the table with the given id to the product with the given id.
     *
     * @param productId
     *            the given productId value.
     * @param dataTableId
     *            the table id
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data.
     */
    void associateTableToProduct(long productId, long dataTableId) throws DataSetProcessingException;

    /**
     * Associates the table with the given id to the dataset with the given id.
     *
     * @param dataSetId
     *            the given datasetId value
     * @param dataTableId
     *            the table id
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data.
     */
    void associateTableToDataSet(long dataSetId, long dataTableId) throws DataSetProcessingException;

    /**
     * Inserts the given object into persistence.
     *
     * @param mapImage
     *            the given {@link MapImage} instance
     * @return the id of mapImage
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data
     * @since 1.3
     */
    long insertMapImage(MapImage mapImage) throws DataSetProcessingException;

    /**
     * Inserts the given MapImage into persistence and links it with its parent DataSet with the
     * given identifier.
     *
     * @param mapImage
     *            the given {@link MapImage} instance
     * @param dataSetId
     *            the dataSetId to create the link to
     * @return the id of mapImage
     * @throws DataSetProcessingException
     *             if there is an error while persisting the data
     * @since 1.4
     */
    long insertMapImage(MapImage mapImage, long dataSetId) throws DataSetProcessingException;

    /**
     * It is expected that ll the LROC DataSets have one or more Volumes; each of those Volumes
     * should make reference to exactly one Mission across the universe of LROC DataSets.
     * <p/>
     * This method will take an LROC data set name, and resolve the mission name that relates to it.
     * If no Mission exists at the time the lookup is made, an exception is raised; it is expected
     * that the LROC Volumes (and hence Mission) will have already been loaded by the time this
     * method is called.
     * <p/>
     * One LROC DataSet may contain multiple Volumes. When the {@link DataSetProcessor} imports
     * these volumes (for whatever reason), it creates one DataSet entity for every volume (instead
     * of re-using the same DataSet entity).
     * <p/>
     * Because of this, given the name of an LROC data set, there might be multiple DataSet entities
     * in the.
     * </p>
     * This method is expected to return the same data set id for a given name consistently, even if
     * multiple entities exist.
     * 
     * @param lrocDataSetName
     *            the data set name for the LROC volume
     * @return the LRO mission id.
     */
    LrocIds findLrocIds(String lrocDataSetName);

    /**
     * Inserts a link between the given dataSetId and the given volumeId
     * @throws DataSetProcessingException 
     */
    void insertDataSetVolume(long dataSetId, long volumeId) throws DataSetProcessingException;
}
