/*
 * Copyright (C) 2011-2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.services.impl;

import gov.nasa.pds.entities.CameraType;
import gov.nasa.pds.entities.DataFile;
import gov.nasa.pds.entities.DataSet;
import gov.nasa.pds.entities.EntityInfo;
import gov.nasa.pds.entities.Instrument;
import gov.nasa.pds.entities.InstrumentHost;
import gov.nasa.pds.entities.MapImage;
import gov.nasa.pds.entities.Mission;
import gov.nasa.pds.entities.Page;
import gov.nasa.pds.entities.PagedResults;
import gov.nasa.pds.entities.Reference;
import gov.nasa.pds.entities.Restriction;
import gov.nasa.pds.entities.SearchCriteria;
import gov.nasa.pds.entities.SearchResults;
import gov.nasa.pds.entities.Target;
import gov.nasa.pds.entities.TargetType;
import gov.nasa.pds.entities.Volume;
import gov.nasa.pds.services.DataSetProcessingConfigurationException;
import gov.nasa.pds.services.DataSetProcessingException;
import gov.nasa.pds.services.DataSetService;
import gov.nasa.pds.services.EntitiesStats;

import java.io.File;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;

import com.topcoder.commons.utils.LoggingWrapperUtility;
import com.topcoder.commons.utils.ValidationUtility;
import com.topcoder.util.log.Log;

/**
 * <p>
 * A comprehensive service implementation to get data for the users.
 * </p>
 *
 * <p>
 * <b>Thread Safety:</b> The implementations are effectively thread-safe.
 * </p>
 *
 * <p>
 * Version 1.1 changes [NASA LMMP - PDS API Update Module Assembly]:
 * <ol>
 * <li>Added full support for LRO map image data.</li>
 * </ol>
 * </p>
 * <p>
 * Version 1.2 changes [NASA PDS API - LROC Update]:
 * <ol>
 * <li>Added {@link WhereSqlAndArgs} type</li>
 * <li>Extracted SQL creation logic from {@link #searchDataSetsByCriteria(SearchCriteria, Page)} and put it into
 * {@link #createSearchByCriteriaSql(String, WhereSqlAndArgs, StringBuilder)},
 * {@link #createInnerJoinSqlForSearchCriteria(SearchCriteria, WhereSqlAndArgs)} and
 * {@link #createWhereSqlAndArgsForSearchCriteria(SearchCriteria)}</li>
 * <li>Introduced {@link #searchMapImagesByCriteria(SearchCriteria, Page)}</li>
 * </ol>
 * </p>
 * 
 * <p>
 * Version 1.3 changes [NASA LMMP API Integration with PDS API]:
 * <ol>
 * <li>Added a very fine grained method: {@link #getMapImagePaths(List)}</li>
 * </ol>
 * </p>
 * 
 * @author argolite, KennyAlive, fivestarwy, caoweiquan322, schmoel
 * @version 1.3
 */
public class JDBCDataSetService implements DataSetService, InitializingBean {
    /**
     * Convenience class that encapsulates both a SearchCriteria method's "where" SQL and the corresponding arguments to
     * bind to the SQL.
     */
    static class WhereSqlAndArgs {
        private StringBuilder whereSql = new StringBuilder();
        private List<Object> args = new ArrayList<Object>();

        public void append(String sql, Object arg) {
            whereSql.append(sql);
            args.add(arg);
        }

        public StringBuilder getWhereSql() {
            return whereSql;
        }

        public List<Object> getArgs() {
            return args;
        }
    }

    /**
     * Used to search for up to 10 map image paths.
     */
    private static final String GET_MAP_IMAGE_PATH_SQL = "select image_path from map_image where id in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
    
    /**
     * Used to fetch all the product types we have in the map_image table.
     */
    private static final String GET_ALL_PROCUCT_TYPES = "select distinct product_type from map_image";
    
    /**
     * Used to fetch all the camera specs we have in the map_image table.
     */
    private static final String GET_ALL_CAMERA_SPECS = "select distinct camera_spec from map_image";

    /**
     * Represents the class name.
     */
    private static final String CLASS_NAME = JDBCDataSetService.class.getName();

    /**
     * Gets DB table name for the given entity type.
     */
    private static final Map<Class<?>, String> ENTITY_TABLE_NAME;

    /**
     * Initializes {@code ENTITY_TABLE_NAME}.
     */
    static {
        Map<Class<?>, String> map = new HashMap<Class<?>, String>();
        map.put(TargetType.class, "target_type");
        map.put(Target.class, "target");
        map.put(Mission.class, "mission");
        map.put(Instrument.class, "instrument");
        map.put(DataSet.class, "dataset");
        map.put(DataFile.class, "data_file");
        map.put(MapImage.class, "map_image");

        ENTITY_TABLE_NAME = Collections.unmodifiableMap(map);
    }

    /**
     * Gets DB row mapper for the given entity type.
     */
    private static final Map<Class<?>, RowMapper<?>> ROW_MAPPERS;

    static {
        Map<Class<?>, RowMapper<?>> map = new HashMap<Class<?>, RowMapper<?>>();

        map.put(TargetType.class, new RowMapper<TargetType>() {
            @Override
            public TargetType mapRow(ResultSet rs, int rowNum) throws SQLException {
                TargetType targetType = new TargetType();
                targetType.setId(rs.getLong("id"));
                targetType.setName(rs.getString("name"));
                return targetType;
            }
        });

        map.put(Target.class, new RowMapper<Target>() {
            @Override
            public Target mapRow(ResultSet rs, int rowNum) throws SQLException {
                Target target = new Target();
                target.setId(rs.getLong("id"));
                target.setName(rs.getString("name"));
                return target;
            }
        });

        map.put(Mission.class, new RowMapper<Mission>() {
            @Override
            public Mission mapRow(ResultSet rs, int rowNum) throws SQLException {
                Mission mission = new Mission();
                mission.setId(rs.getLong("id"));
                mission.setName(rs.getString("name"));
                mission.setStartDate(rs.getDate("start_date"));
                mission.setEndDate(rs.getDate("end_date"));
                mission.setDescription(rs.getString("description"));
                return mission;
            }
        });

        map.put(Instrument.class, new RowMapper<Instrument>() {
            @Override
            public Instrument mapRow(ResultSet rs, int rowNum) throws SQLException {
                Instrument instrument = new Instrument();
                instrument.setId(rs.getLong("id"));
                instrument.setName(rs.getString("name"));
                instrument.setTextId(rs.getString("instrument_text_id"));
                instrument.setType(rs.getString("type"));
                instrument.setDescription(rs.getString("description"));
                return instrument;
            }
        });

        map.put(DataSet.class, new RowMapper<DataSet>() {
            @Override
            public DataSet mapRow(ResultSet rs, int rowNum) throws SQLException {
                DataSet dataSet = new DataSet();
                dataSet.setId(rs.getLong("id"));
                dataSet.setName(rs.getString("name"));
                dataSet.setTextId(rs.getString("data_set_text_id"));
                dataSet.setStartDate(rs.getTimestamp("start_time"));
                dataSet.setStopDate(rs.getTimestamp("stop_time"));
                dataSet.setDescription(rs.getString("description"));
                return dataSet;
            }
        });

        map.put(DataFile.class, new RowMapper<DataFile>() {
            @Override
            public DataFile mapRow(ResultSet rs, int rowNum) throws SQLException {
                DataFile dataFile = new DataFile();
                dataFile.setId(rs.getLong("id"));
                dataFile.setName(rs.getString("name"));
                dataFile.setPath(rs.getString("path"));
                dataFile.setContent(rs.getString("content"));
                return dataFile;
            }
        });

        map.put(Reference.class, new RowMapper<Reference>() {
            @Override
            public Reference mapRow(ResultSet rs, int rowNum) throws SQLException {
                Reference reference = new Reference();
                reference.setId(rs.getLong("id"));
                reference.setKeyTextId(rs.getString("reference_key_text_id"));
                reference.setDescription(rs.getString("description"));
                return reference;
            }
        });

        map.put(InstrumentHost.class, new RowMapper<InstrumentHost>() {
            @Override
            public InstrumentHost mapRow(ResultSet rs, int rowNum) throws SQLException {
                InstrumentHost instrumentHost = new InstrumentHost();
                instrumentHost.setId(rs.getLong("id"));
                instrumentHost.setName(rs.getString("name"));
                instrumentHost.setTextId(rs.getString("instrument_host_text_id"));
                return instrumentHost;
            }
        });

        map.put(Volume.class, new RowMapper<Volume>() {
            @Override
            public Volume mapRow(ResultSet rs, int rowNum) throws SQLException {
                Volume volume = new Volume();
                volume.setId(rs.getLong("id"));
                volume.setName(rs.getString("name"));
                volume.setDescription(rs.getString("description"));
                volume.setTextId(rs.getString("volume_text_id"));
                volume.setSetTextId(rs.getString("volume_set_text_id"));
                volume.setSetName(rs.getString("volume_set_name"));
                volume.setSeriesName(rs.getString("volume_series_name"));
                return volume;
            }
        });

        map.put(MapImage.class, new RowMapper<MapImage>() {
            @Override
            public MapImage mapRow(ResultSet rs, int rowNum) throws SQLException {
                MapImage mapImage = new MapImage();
                mapImage.setId(rs.getLong("id"));
                mapImage.setName(rs.getString("name"));
                mapImage.setMissionId(rs.getLong("mission_id"));
                mapImage.setImagePath(rs.getString("image_path"));
                mapImage.setDate(rs.getDate("date"));
                mapImage.setCenterLongitude(rs.getDouble("center_longitude"));
                mapImage.setCenterLatitude(rs.getDouble("center_latitude"));
                mapImage.setIllumination(rs.getDouble("illumination"));
                mapImage.setCameraAngle(rs.getDouble("camera_angle"));
                try {
                    mapImage.setCameraType(CameraType.valueOf(rs.getString("camera_type")));
                } catch (IllegalArgumentException e) {
                    throw new SQLException("The field camera_type exists but is not a legal CameraType", e);
                }
                return mapImage;
            }
        });

        ROW_MAPPERS = Collections.unmodifiableMap(map);
    }

    /**
     * Represents the log instance used for logging.
     */
    private Log logger;

    /**
     * Represents the JdbcTemplate instance used for all DB interaction.
     */
    private JdbcTemplate jdbcTemplate;

    /**
     * Represents the numeric threshold below which a rating is deemed to be insufficient, hence a white spot.
     */
    private float whiteSpotRatingThreshold;

    /**
     * Represents the percent threshold below which a rating is deemed to be insufficient, hence a white spot. This
     * rating will be measured respective to the 'best' rating in the system.
     */
    private float whiteSpotRatingPercentThreshold;

    /**
     * Represents the number of popular queries to return when requested.
     */
    private int popularQueryRetrievalThreshold;

    private Map<String, String> dataFilesMapping;

    private String previewImagesDirectory;

    private String previewImagesURLPrefix;

    /**
     * Creates new JDBCDataSetService instance.
     */
    public JDBCDataSetService() {
        // Empty
    }

    /**
     * Sets the log instance used for logging.
     * 
     * @param logger
     *            the log instance used for logging
     */
    public void setLogger(Log logger) {
        this.logger = logger;
    }

    /**
     * Sets the JdbcTemplate instance used for all DB interaction.
     * 
     * @param jdbcTemplate
     *            the JdbcTemplate instance used for all DB interaction
     */
    public void setJdbcTemplate(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    /**
     * Sets the numeric threshold below which a rating is deemed to be insufficient, hence a white spot.
     * 
     * @param whiteSpotRatingThreshold
     *            the numeric threshold below which a rating is deemed to be insufficient, hence a white spot
     */
    public void setWhiteSpotRatingThreshold(float whiteSpotRatingThreshold) {
        this.whiteSpotRatingThreshold = whiteSpotRatingThreshold;
    }

    /**
     * Sets the percent threshold below which a rating is deemed to be insufficient.
     * 
     * @param whiteSpotRatingPercentThreshold
     *            the percent threshold below which a rating is deemed to be insufficient
     */
    public void setWhiteSpotRatingPercentThreshold(float whiteSpotRatingPercentThreshold) {
        this.whiteSpotRatingPercentThreshold = whiteSpotRatingPercentThreshold;
    }

    /**
     * Sets the number of popular queries to return when requested.
     * 
     * @param popularQueryRetrievalThreshold
     *            the number of popular queries to return when requested
     */
    public void setPopularQueryRetrievalThreshold(int popularQueryRetrievalThreshold) {
        this.popularQueryRetrievalThreshold = popularQueryRetrievalThreshold;
    }

    public void setDataFilesMapping(Map<String, String> dataFilesMapping) {
        this.dataFilesMapping = dataFilesMapping;
    }

    public void setPreviewImagesDirectory(String previewImagesDirectory) {
        this.previewImagesDirectory = previewImagesDirectory;
    }

    public void setPreviewImagesURLPrefix(String previewImagesURLPrefix) {
        if (!previewImagesURLPrefix.endsWith("/")) {
            this.previewImagesURLPrefix = previewImagesURLPrefix + "/";
        } else {
            this.previewImagesURLPrefix = previewImagesURLPrefix;
        }
    }

    /**
     * Checks whether this class was initialized by Spring properly.
     *
     * Required parameters:
     * <ul>
     * <li>logger</li>
     * <li>jdbcTemplate</li>
     * <li>whiteSpotRatingThreshold</li>
     * <li>whiteSpotRatingPercentThreshold</li>
     * <li>popularQueryRetrievalThreshold</li>
     * </ul>
     *
     * @throws DataSetProcessingConfigurationException
     *             if logger or jdbcTemplate or whiteSpotRatingThreshold or whiteSpotRatingPercentThreshold or
     *             adHocQueryTranslator or popularQueryRetrievalThreshold are null
     */
    @Override
    public void afterPropertiesSet() {
        ValidationUtility.checkNotNull(logger, "logger", DataSetProcessingConfigurationException.class);
        ValidationUtility.checkNotNull(jdbcTemplate, "jdbcTemplate", DataSetProcessingConfigurationException.class);
        ValidationUtility.checkNotNull(whiteSpotRatingThreshold, "whiteSpotRatingThreshold",
                DataSetProcessingConfigurationException.class);
        ValidationUtility.checkNotNull(whiteSpotRatingPercentThreshold, "whiteSpotRatingPercentThreshold",
                DataSetProcessingConfigurationException.class);
        ValidationUtility.checkNotNull(popularQueryRetrievalThreshold, "popularQueryRetrievalThreshold",
                DataSetProcessingConfigurationException.class);
    }

    /* ******************************************************************************************** */
    /* Target Type operations */
    /* ******************************************************************************************** */
    /**
     * {@inheritDoc}
     */
    @Override
    public PagedResults<EntityInfo> getTargetTypesInfo(Page page) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getTargetTypesInfo(Page page)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "page" }, new Object[] { page });

        String sql = "select id, name from target_type";

        PagedResults<EntityInfo> result = getEntitiesInfo(signature, page, "target_type", sql, null);
        LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TargetType getTargetType(long targetTypeId) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getTargetType(long targetTypeId)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "targetTypeId" },
                new Object[] { targetTypeId });

        try {
            TargetType result = getEntity(TargetType.class, targetTypeId);

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
            return result;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get target type entity", e));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EntitiesStats getTargetTypeChildrenStats(long targetTypeId) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getTargetTypeChildrenStats(long targetTypeId)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "targetTypeId" },
                new Object[] { targetTypeId });

        try {
            EntitiesStats stats = new EntitiesStats();

            stats.setTargetsCount(jdbcTemplate.queryForInt(
                    "select count(distinct target_id) from target_type_target where target_type_id = ?", targetTypeId));

            stats.setMissionsCount(jdbcTemplate.queryForInt(
                    "select count(distinct mission_id) from dataset_mission DM "
                            + "inner join dataset_target DT on DM.dataset_id = DT.dataset_id "
                            + "inner join target_type_target TTT on DT.target_id = TTT.target_id "
                            + "where TTT.target_type_id = ?", targetTypeId));

            stats.setInstrumentsCount(jdbcTemplate.queryForInt(
                    "select count(distinct instrument_id) from instrument_catalog IC "
                            + "inner join dataset_target DT on IC.dataset_id = DT.dataset_id "
                            + "inner join target_type_target TTT on DT.target_id = TTT.target_id "
                            + "where TTT.target_type_id = ?", targetTypeId));

            stats.setDataSetsCount(jdbcTemplate.queryForInt("select count(distinct dataset_id) from dataset_target DT "
                    + "inner join target_type_target TTT on DT.target_id = TTT.target_id "
                    + "where TTT.target_type_id = ?", targetTypeId));

            stats.setDocumentsCount(jdbcTemplate.queryForInt(
                    "select count(distinct data_file_id) from dataset_file DF "
                            + "inner join dataset_target DT on DF.dataset_id = DT.dataset_id "
                            + "inner join target_type_target TTT on DT.target_id = TTT.target_id "
                            + "where TTT.target_type_id = ?", targetTypeId));

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { stats });
            return stats;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get target type children stats", e));
        }
    }

    /* ******************************************************************************************** */
    /* Target operations */
    /* ******************************************************************************************** */
    /**
     * {@inheritDoc}
     */
    @Override
    public PagedResults<EntityInfo> getTargetsInfo(Page page, Restriction restriction)
            throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getTargetsInfo(Page page, Restriction restriction)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "page", "restriction" }, new Object[] {
                page, restriction });

        PagedResults<EntityInfo> result = getTargetsInfoInternal(null, signature, page, restriction);
        LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
        return result;
    }

    private PagedResults<EntityInfo> getTargetsInfoInternal(String searchTextOrNull, String signature, Page page,
            Restriction restriction) throws DataSetProcessingException {

        checkRestriction(signature, restriction, TargetType.class);

        String sql = null;
        List<Object> args = null;
        if (restriction == null) {
            sql = "select T.id, T.name from target T";
        } else {
            if (restriction.getRestrictionEntityClass() == TargetType.class) {
                sql = "select T.id, T.name from target T "
                        + "inner join target_type_target TTT on T.id = TTT.target_id " + "where TTT.target_type_id = ?";
            }
            if (searchTextOrNull != null) {
                sql += " and T.name like '%" + searchTextOrNull + "%'";
            }
            args = Arrays.asList((Object) restriction.getRestrictionEntityId());
        }
        return getEntitiesInfo(signature, page, "T", sql, args);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Target getTarget(long targetId) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getTarget(long targetId)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "targetId" }, new Object[] { targetId });

        try {
            Target result = getEntity(Target.class, targetId);

            // Get Target's dependent entities
            if (result != null) {
                // get types
                String sql = "select * from target_type TT inner join target_type_target TTT "
                        + "on TT.id = TTT.target_type_id where TTT.target_id = ?";

                @SuppressWarnings("unchecked")
                RowMapper<TargetType> rowMapper = (RowMapper<TargetType>) ROW_MAPPERS.get(TargetType.class);

                List<TargetType> targetTypes = jdbcTemplate.query(sql, rowMapper, targetId);
                result.setTypes(targetTypes);

                // get references
                sql = "select * from reference R inner join target_reference TR "
                        + "on R.id = TR.reference_id where TR.target_id = ?";

                @SuppressWarnings("unchecked")
                RowMapper<Reference> rowMapper2 = (RowMapper<Reference>) ROW_MAPPERS.get(Reference.class);

                List<Reference> references = jdbcTemplate.query(sql, rowMapper2, targetId);
                result.setReferences(references);
            }

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
            return result;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get target entity", e));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EntitiesStats getTargetChildrenStats(long targetId) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getTargetChildrenStats(long targetId)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "targetId" }, new Object[] { targetId });

        try {
            EntitiesStats stats = new EntitiesStats();

            stats.setMissionsCount(jdbcTemplate.queryForInt(
                    "select count(distinct mission_id) from dataset_mission DM "
                            + "inner join dataset_target DT on DM.dataset_id = DT.dataset_id "
                            + "where DT.target_id = ?", targetId));

            stats.setInstrumentsCount(jdbcTemplate.queryForInt(
                    "select count(distinct instrument_id) from instrument_catalog IC "
                            + "inner join dataset_target DT on IC.dataset_id = DT.dataset_id "
                            + "where DT.target_id = ?", targetId));

            stats.setDataSetsCount(jdbcTemplate.queryForInt("select count(distinct dataset_id) from dataset_target DT "
                    + "where DT.target_id = ?", targetId));

            stats.setDocumentsCount(jdbcTemplate.queryForInt(
                    "select count(distinct data_file_id) from dataset_file DF "
                            + "inner join dataset_target DT on DF.dataset_id = DT.dataset_id "
                            + "where DT.target_id = ?", targetId));

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { stats });
            return stats;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get target children stats", e));
        }
    }

    /* ******************************************************************************************** */
    /* Mission operations */
    /* ******************************************************************************************** */
    /**
     * {@inheritDoc}
     */
    @Override
    public PagedResults<EntityInfo> getMissionsInfo(Page page, Restriction restriction)
            throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getMissionsInfo(Page page, Restriction restriction)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "page", "restriction" }, new Object[] {
                page, restriction });

        PagedResults<EntityInfo> result = getMissionsInfoInternal(null, signature, page, restriction);
        LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
        return result;
    }

    private PagedResults<EntityInfo> getMissionsInfoInternal(String searchTextOrNull, String signature, Page page,
            Restriction restriction) throws DataSetProcessingException {

        checkRestriction(signature, restriction, TargetType.class, Target.class);

        String sql = null;
        List<Object> args = null;
        if (restriction == null) {
            sql = "select M.id, M.name from mission M";
        } else {
            if (restriction.getRestrictionEntityClass() == Target.class) {
                sql = "select distinct M.id, M.name from mission M "
                        + "inner join dataset_mission DM on M.id = DM.mission_id "
                        + "inner join dataset_target DT on DM.dataset_id = DT.dataset_id " + "where DT.target_id = ?";
            } else if (restriction.getRestrictionEntityClass() == TargetType.class) {
                sql = "select distinct M.id, M.name from mission M "
                        + "inner join dataset_mission DM on M.id = DM.mission_id "
                        + "inner join dataset_target DT on DM.dataset_id = DT.dataset_id "
                        + "inner join target_type_target TTT on DT.target_id = TTT.target_id "
                        + "where TTT.target_type_id = ?";
            }
            if (searchTextOrNull != null) {
                sql += " and M.name like '%" + searchTextOrNull + "%'";
            }
            args = Arrays.asList((Object) restriction.getRestrictionEntityId());
        }
        return getEntitiesInfo(signature, page, "M", sql, args);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Mission getMission(long missionId) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getMission(long missionId)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "missionId" }, new Object[] { missionId });

        try {
            Mission result = getEntity(Mission.class, missionId);

            if (result != null) {
                String sql = "select * from reference R inner join mission_reference MR "
                        + "on R.id = MR.reference_id where MR.mission_id = ?";

                @SuppressWarnings("unchecked")
                RowMapper<Reference> rowMapper = (RowMapper<Reference>) ROW_MAPPERS.get(Reference.class);

                List<Reference> references = jdbcTemplate.query(sql, rowMapper, missionId);
                result.setReferences(references);
            }

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
            return result;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get mission entity", e));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EntitiesStats getMissionChildrenStats(long missionId) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getMissionChildrenStats(long missionId)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "missionId" }, new Object[] { missionId });

        try {
            EntitiesStats stats = new EntitiesStats();

            stats.setInstrumentsCount(jdbcTemplate.queryForInt(
                    "select count(distinct instrument_id) from instrument_catalog IC "
                            + "inner join dataset_mission DM on IC.dataset_id = DM.dataset_id "
                            + "where DM.mission_id = ?", missionId));

            stats.setDataSetsCount(jdbcTemplate.queryForInt(
                    "select count(dataset_id) from dataset_mission where mission_id = ?", missionId));

            stats.setDocumentsCount(jdbcTemplate.queryForInt(
                    "select count(distinct data_file_id) from dataset_file DF "
                            + "inner join dataset_mission DM on DF.dataset_id = DM.dataset_id "
                            + "where DM.mission_id = ?", missionId));

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { stats });
            return stats;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get mission children stats", e));
        }
    }

    /* ******************************************************************************************** */
    /* Instrument operations */
    /* ******************************************************************************************** */
    /**
     * {@inheritDoc}
     */
    @Override
    public PagedResults<EntityInfo> getInstrumentsInfo(Page page, Restriction restriction)
            throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getInstrumentsInfo(Page page, Restriction restriction)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "page", "restriction" }, new Object[] {
                page, restriction });

        PagedResults<EntityInfo> result = getInstrumentsInfoInternal(null, signature, page, restriction);
        LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
        return result;
    }

    private PagedResults<EntityInfo> getInstrumentsInfoInternal(String searchTextOrNull, String signature, Page page,
            Restriction restriction) throws DataSetProcessingException {

        checkRestriction(signature, restriction, Mission.class, Target.class, TargetType.class);

        String sql = null;
        List<Object> args = null;
        if (restriction == null) {
            sql = "select I.id, I.name from instrument I";
        } else {
            if (restriction.getRestrictionEntityClass() == Mission.class) {
                sql = "select distinct I.id, I.name from instrument I "
                        + "inner join instrument_catalog IC on I.id = IC.instrument_id "
                        + "inner join dataset_mission DM on IC.dataset_id = DM.dataset_id " + "where DM.mission_id = ?";
            } else if (restriction.getRestrictionEntityClass() == Target.class) {
                sql = "select distinct I.id, I.name from instrument I "
                        + "inner join instrument_catalog IC on I.id = IC.instrument_id "
                        + "inner join dataset_target DT on IC.dataset_id = DT.dataset_id " + "where DT.target_id = ?";
            } else if (restriction.getRestrictionEntityClass() == TargetType.class) {
                sql = "select distinct I.id, I.name from instrument I "
                        + "inner join instrument_catalog IC on I.id = IC.instrument_id "
                        + "inner join dataset_target DT on IC.dataset_id = DT.dataset_id "
                        + "inner join target_type_target TTT on DT.target_id = TTT.target_id "
                        + "where TTT.target_type_id = ?";
            }
            if (searchTextOrNull != null) {
                sql += " and I.name like '%" + searchTextOrNull + "%'";
            }
            args = Arrays.asList((Object) restriction.getRestrictionEntityId());
        }
        return getEntitiesInfo(signature, page, "I", sql, args);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Instrument getInstrument(long instrumentId) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getInstrument(long instrumentId)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "instrumentId" },
                new Object[] { instrumentId });

        try {
            Instrument result = getEntity(Instrument.class, instrumentId);

            if (result != null) {
                String sql = "select * from reference R inner join instrument_reference IR "
                        + "on R.id = IR.reference_id where IR.instrument_id = ?";
                @SuppressWarnings("unchecked")
                RowMapper<Reference> referenceRowMapper = (RowMapper<Reference>) ROW_MAPPERS.get(Reference.class);
                List<Reference> references = jdbcTemplate.query(sql, referenceRowMapper, instrumentId);
                result.setReferences(references);

                sql = "select * from instrument_host IH inner join instrument_host_instrument IHI "
                        + "on IH.id = IHI.instrument_host_id where IHI.instrument_id = ?";
                @SuppressWarnings("unchecked")
                RowMapper<InstrumentHost> hostRowMapper = (RowMapper<InstrumentHost>) ROW_MAPPERS
                        .get(InstrumentHost.class);
                List<InstrumentHost> hosts = jdbcTemplate.query(sql, hostRowMapper, instrumentId);
                result.setHosts(hosts);
            }

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
            return result;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get instrument entity", e));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EntitiesStats getInstrumentChildrenStats(long instrumentId) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getInstrumentChildrenStats(long instrumentId)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "instrumentId" },
                new Object[] { instrumentId });

        try {
            EntitiesStats stats = new EntitiesStats();

            stats.setDataSetsCount(jdbcTemplate.queryForInt(
                    "select count(distinct dataset_id) from instrument_catalog where instrument_id = ?", instrumentId));

            stats.setDocumentsCount(jdbcTemplate.queryForInt(
                    "select count(distinct data_file_id) from dataset_file DF "
                            + "inner join instrument_catalog IC on DF.dataset_id = IC.dataset_id "
                            + "where IC.instrument_id = ?", instrumentId));

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { stats });
            return stats;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get instrument children stats", e));
        }
    }

    /* ******************************************************************************************** */
    /* DataSet operations */
    /* ******************************************************************************************** */
    /**
     * {@inheritDoc}
     */
    @Override
    public PagedResults<EntityInfo> getDataSetsInfo(Page page, Restriction restriction)
            throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getDataSetsInfo(Page page, Restriction restriction)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "page", "restriction" }, new Object[] {
                page, restriction });

        PagedResults<EntityInfo> result = getDataSetsInfoInternal(null, signature, page, restriction);
        LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
        return result;
    }

    private PagedResults<EntityInfo> getDataSetsInfoInternal(String searchTextOrNull, String signature, Page page,
            Restriction restriction) throws DataSetProcessingException {

        checkRestriction(signature, restriction, Instrument.class, Mission.class, Target.class, TargetType.class);

        String sql = null;
        List<Object> args = null;
        if (restriction == null) {
            sql = "select DS.id, DS.name from dataset DS";
        } else {
            if (restriction.getRestrictionEntityClass() == Instrument.class) {
                sql = "select distinct DS.id, DS.name from dataset DS "
                        + "inner join instrument_catalog IC on DS.id = IC.dataset_id " + "where IC.instrument_id = ?";
            } else if (restriction.getRestrictionEntityClass() == Mission.class) {
                sql = "select distinct DS.id, DS.name from dataset DS "
                        + "inner join dataset_mission DM on DS.id = DM.dataset_id " + "where DM.mission_id = ?";
            } else if (restriction.getRestrictionEntityClass() == Target.class) {
                sql = "select distinct DS.id, DS.name from dataset DS "
                        + "inner join dataset_target DT on DS.id = DT.dataset_id " + "where DT.target_id = ?";
            } else if (restriction.getRestrictionEntityClass() == TargetType.class) {
                sql = "select distinct DS.id, DS.name from dataset DS "
                        + "inner join dataset_target DT on DS.id = DT.dataset_id "
                        + "inner join target_type_target TTT on DT.target_id = TTT.target_id "
                        + "where TTT.target_type_id = ?";
            }
            if (searchTextOrNull != null) {
                sql += " and DS.name like '%" + searchTextOrNull + "%'";
            }
            args = Arrays.asList((Object) restriction.getRestrictionEntityId());
        }
        return getEntitiesInfo(signature, page, "DS", sql, args);
    }

    /**
     * <p>
     * Get the dataset from persistence.
     * </p>
     *
     * @param dataSetId
     *            the dataSet id.
     * @return the dataset with given id.
     * @throws DataSetProcessingException
     *             if there is any error.
     */
    @Override
    public DataSet getDataSet(long dataSetId) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getDataSet(long dataSetId)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "dataSetId" }, new Object[] { dataSetId });

        try {
            DataSet result = getEntity(DataSet.class, dataSetId);

            if (result != null) {
                String sql = "select * from volume V inner join dataset_volume DV "
                        + "on V.id = DV.volume_id where DV.dataset_id = ?";
                @SuppressWarnings("unchecked")
                RowMapper<Volume> volumeRowMapper = (RowMapper<Volume>) ROW_MAPPERS.get(Volume.class);
                List<Volume> volumes = jdbcTemplate.query(sql, volumeRowMapper, dataSetId);
                result.setVolumes(volumes);

                sql = "select * from target T inner join dataset_target DT "
                        + "on T.id = DT.target_id where DT.dataset_id = ?";
                @SuppressWarnings("unchecked")
                RowMapper<Target> targetRowMapper = (RowMapper<Target>) ROW_MAPPERS.get(Target.class);
                List<Target> targets = jdbcTemplate.query(sql, targetRowMapper, dataSetId);
                result.setTargets(targets);

                sql = "select * from mission M inner join dataset_mission DM "
                        + "on M.id = DM.mission_id where DM.dataset_id = ?";
                @SuppressWarnings("unchecked")
                RowMapper<Mission> missionRowMapper = (RowMapper<Mission>) ROW_MAPPERS.get(Mission.class);
                List<Mission> missions = jdbcTemplate.query(sql, missionRowMapper, dataSetId);
                result.setMissions(missions);

                sql = "select * from instrument I inner join instrument_catalog IC "
                        + "on I.id = IC.instrument_id where IC.dataset_id = ?";
                @SuppressWarnings("unchecked")
                RowMapper<Instrument> instrumentRowMapper = (RowMapper<Instrument>) ROW_MAPPERS.get(Instrument.class);
                List<Instrument> instruments = jdbcTemplate.query(sql, instrumentRowMapper, dataSetId);
                result.setInstruments(instruments);

                sql = "select * from reference R inner join reference_catalog RC "
                        + "on R.id = RC.reference_id where RC.dataset_id = ?";
                @SuppressWarnings("unchecked")
                RowMapper<Reference> referenceRowMapper = (RowMapper<Reference>) ROW_MAPPERS.get(Reference.class);
                List<Reference> references = jdbcTemplate.query(sql, referenceRowMapper, dataSetId);
                result.setReferences(references);

                sql = "select * from map_image MI inner join dataset_map_image DMI "
                        + "on MI.id = DMI.map_image_id where DMI.dataset_id = ?";
                @SuppressWarnings("unchecked")
                RowMapper<MapImage> mapImageRowMapper = (RowMapper<MapImage>) ROW_MAPPERS.get(MapImage.class);
                List<MapImage> mapImages = jdbcTemplate.query(sql, mapImageRowMapper, dataSetId);
                result.setMapImages(mapImages);
            }

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
            return result;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get dataset entity", e));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EntitiesStats getDataSetChildrenStats(long dataSetId) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getDataSetStats(long dataSetId)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "dataSetId" }, new Object[] { dataSetId });

        try {
            EntitiesStats stats = new EntitiesStats();

            stats.setDocumentsCount(jdbcTemplate.queryForInt("select count(*) from dataset_file where dataset_id = ?",
                    dataSetId));

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { stats });
            return stats;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get dataset children stats", e));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<EntityInfo> getDataSetRelatedEntitites(long dataSetId, Class<?> entityClass)
            throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getDataSetRelatedEntitites(long dataSetId, Class<?> entityClass)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "dataSetId", "entityClass" }, new Object[] {
                dataSetId, entityClass });

        try {
            String sql, tableName;
            if (entityClass == TargetType.class) {
                sql = "select distinct(TT.id), TT.name from target_type TT "
                        + "inner join target_type_target TTT on TT.id = TTT.target_type_id "
                        + "inner join dataset_target DT on TTT.target_id = DT.target_id " + "where DT.dataset_id = ?";
                tableName = "TT";
            } else if (entityClass == Target.class) {
                sql = "select distinct(T.id), T.name from target T inner join dataset_target DT on T.id = DT.target_id "
                        + "where DT.dataset_id = ?";
                tableName = "T";
            } else if (entityClass == Mission.class) {
                sql = "select distinct(M.id), M.name from mission M inner join dataset_mission DM on M.id = DM.mission_id "
                        + "where DM.dataset_id = ?";
                tableName = "M";
            } else if (entityClass == Instrument.class) {
                sql = "select distinct(I.id), I.name from instrument I inner join instrument_catalog IC on I.id = IC.instrument_id "
                        + "where IC.dataset_id = ?";
                tableName = "I";
            } else {
                throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                        "Failed to get dataset related entities for unsupported entity class"));
            }

            List<Object> args = Arrays.asList((Object) dataSetId);
            List<EntityInfo> result = getEntitiesInfo(signature, null, tableName, sql, args).getResults();
            LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
            return result;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get dataset related entities", e));
        }
    }

    /* ******************************************************************************************** */
    /* DataFile operations */
    /* ******************************************************************************************** */
    /**
     * {@inheritDoc}
     */
    @Override
    public PagedResults<EntityInfo> getDataFilesInfo(Page page, Restriction restriction)
            throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getDataFilesInfo(Page page, Restriction restriction)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "page", "restriction" }, new Object[] {
                page, restriction });

        PagedResults<EntityInfo> result = getDataFilesInfoInternal(null, signature, page, restriction);
        LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
        return result;
    }

    private PagedResults<EntityInfo> getDataFilesInfoInternal(String searchTextOrNull, String signature, Page page,
            Restriction restriction) throws DataSetProcessingException {

        checkRestriction(signature, restriction, DataSet.class, Instrument.class, Mission.class, Target.class,
                TargetType.class);

        String sql = null;
        List<Object> args = null;
        if (restriction == null) {
            sql = "select DF.id, DF.name from data_file DF";
        } else {
            if (restriction.getRestrictionEntityClass() == DataSet.class) {
                sql = "select DF.id, DF.name from data_file DF "
                        + "inner join dataset_file DSF on DF.id = DSF.data_file_id " + "where DSF.dataset_id = ?";
            } else if (restriction.getRestrictionEntityClass() == Instrument.class) {
                sql = "select DF.id, DF.name from data_file DF "
                        + "inner join dataset_file DSF on DF.id = DSF.data_file_id "
                        + "inner join instrument_catalog IC on DSF.dataset_id = IC.dataset_id "
                        + "where IC.instrument_id = ?";
            } else if (restriction.getRestrictionEntityClass() == Mission.class) {
                sql = "select DF.id, DF.name from data_file DF "
                        + "inner join dataset_file DSF on DF.id = DSF.data_file_id "
                        + "inner join dataset_mission DM on DSF.dataset_id = DM.dataset_id "
                        + "where DM.mission_id = ?";
            } else if (restriction.getRestrictionEntityClass() == Target.class) {
                sql = "select DF.id, DF.name from data_file DF "
                        + "inner join dataset_file DSF on DF.id = DSF.data_file_id "
                        + "inner join dataset_target DT on DSF.dataset_id = DT.dataset_id " + "where DT.target_id = ?";
            } else if (restriction.getRestrictionEntityClass() == TargetType.class) {
                sql = "select DF.id, DF.name from data_file DF "
                        + "inner join dataset_file DSF on DF.id = DSF.data_file_id "
                        + "inner join dataset_target DT on DSF.dataset_id = DT.dataset_id "
                        + "inner join target_type_target TTT on DT.target_id = TTT.target_id "
                        + "where TTT.target_type_id = ?";
            }
            if (searchTextOrNull != null) {
                sql += " and DF.name like '%" + searchTextOrNull + "%'";
            }
            args = Arrays.asList((Object) restriction.getRestrictionEntityId());
        }
        return getEntitiesInfo(signature, page, "DF", sql, args);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PagedResults<EntityInfo> getImagesInfo(Page page, Restriction restriction) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getImagesInfo(Page page, Restriction restriction)";
        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "page", "restriction" }, new Object[] {
                page, restriction });

        checkRestriction(signature, restriction, DataSet.class, Instrument.class, Mission.class, Target.class,
                TargetType.class);

        String sql = null;
        List<Object> args = null;
        if (restriction == null) {
            sql = "select DF.id, DF.name from data_file DF where DF.name LIKE '_%\\_IMAGE'";
        } else {
            if (restriction.getRestrictionEntityClass() == DataSet.class) {
                sql = "select DF.id, DF.name from data_file DF "
                        + "inner join product_file PF on DF.id = PF.data_file_id "
                        + "inner join product_index PI on PF.product_id = PI.data_product_id "
                        + "where PI.dataset_id = ? and DF.name LIKE '_%\\_IMAGE'";
            } else if (restriction.getRestrictionEntityClass() == Instrument.class) {
                sql = "select DF.id, DF.name from data_file DF "
                        + "inner join product_file PF on DF.id = PF.data_file_id "
                        + "inner join product_index PI on PF.product_id = PI.data_product_id "
                        + "inner join instrument_catalog IC on PI.dataset_id = IC.dataset_id "
                        + "where IC.instrument_id = ?";
            } else if (restriction.getRestrictionEntityClass() == Mission.class) {
                sql = "select DF.id, DF.name from data_file DF "
                        + "inner join product_file PF on DF.id = PF.data_file_id "
                        + "inner join product_index PI on PF.product_id = PI.data_product_id "
                        + "inner join dataset_mission DM on PI.dataset_id = DM.dataset_id " + "where DM.mission_id = ?";
            } else if (restriction.getRestrictionEntityClass() == Target.class) {
                sql = "select DF.id, DF.name from data_file DF "
                        + "inner join product_file PF on DF.id = PF.data_file_id "
                        + "inner join product_index PI on PF.product_id = PI.data_product_id "
                        + "inner join dataset_target DT on PI.dataset_id = DT.dataset_id " + "where DT.target_id = ?";
            } else if (restriction.getRestrictionEntityClass() == TargetType.class) {
                sql = "select DF.id, DF.name from data_file DF "
                        + "inner join product_file PF on DF.id = PF.data_file_id "
                        + "inner join product_index PI on PF.product_id = PI.data_product_id "
                        + "inner join dataset_target DT on PI.dataset_id = DT.dataset_id "
                        + "inner join target_type_target TTT on DT.target_id = TTT.target_id "
                        + "where TTT.target_type_id = ?";
            }
            args = Arrays.asList((Object) restriction.getRestrictionEntityId());
        }

        PagedResults<EntityInfo> result = getEntitiesInfo(signature, page, "DF", sql, args);
        LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
        return result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DataFile getDataFile(long dataFileId) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getDataFile(long dataFileId)";

        LoggingWrapperUtility
                .logEntrance(logger, signature, new String[] { "dataFileId" }, new Object[] { dataFileId });

        try {
            DataFile result = getEntity(DataFile.class, dataFileId);

            if (result.getPath() != null && dataFilesMapping != null) {
                for (Map.Entry<String, String> entry : dataFilesMapping.entrySet()) {
                    if (result.getPath().startsWith(entry.getKey().trim())) {
                        result.setPath(result.getPath().replace(entry.getKey().trim(), entry.getValue().trim()));
                        break;
                    }
                }
            }

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
            return result;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get data file entity", e));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getPreviewImageURL(long imageFileId) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".getPreviewImageURL(long imageFileId)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "imageFileId" },
                new Object[] { imageFileId });

        try {
            DataFile dataFile = getEntity(DataFile.class, imageFileId);

            if (!dataFile.getName().endsWith("_IMAGE") || dataFile.getPath() == null) {
                throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                        "The file for the given id " + imageFileId + " is not an image"));
            }

            String imageURL = null;

            if (dataFilesMapping != null) {
                String path = dataFile.getPath();
                for (Map.Entry<String, String> entry : dataFilesMapping.entrySet()) {
                    String pathPrefix = entry.getKey().trim();
                    if (path.startsWith(pathPrefix)) {
                        int prefixLength = pathPrefix.length();
                        if (path.length() > prefixLength && path.charAt(prefixLength) == '/') {
                            prefixLength++;
                        }
                        int end = path.lastIndexOf('.');
                        if (end == -1) {
                            end = path.length();
                        }
                        String relPath = path.substring(prefixLength, end) + ".png";
                        if (new File(previewImagesDirectory, relPath).exists()) {
                            imageURL = previewImagesURLPrefix + relPath;
                        }
                        break;
                    }
                }
            }
            return imageURL;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get preview image URL", e));
        }
    }

    /* **************************************************************************************
     * Search support *************************************************************************************
     */
    /**
     * {@inheritDoc}
     */
    @Override
    public SearchResults searchEntities(String searchText, Page page) throws DataSetProcessingException {
        String signature = CLASS_NAME + ".searchEntities(String searchText, Page page)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "searchText", "page" }, new Object[] {
                searchText, page });

        try {
            SearchResults result = new SearchResults();
            result.setTargetTypes(searchEntitiesByType(TargetType.class, searchText, page, null));
            result.setTargets(searchEntitiesByType(Target.class, searchText, page, null));
            result.setMissions(searchEntitiesByType(Mission.class, searchText, page, null));
            result.setInstruments(searchEntitiesByType(Instrument.class, searchText, page, null));
            result.setDatasets(searchEntitiesByType(DataSet.class, searchText, page, null));
            result.setDataFiles(searchEntitiesByType(DataFile.class, searchText, page, null));

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
            return result;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to execute search request", e));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PagedResults<EntityInfo> searchEntitiesByType(Class<?> entityClass, String searchText, Page page,
            Restriction restriction) throws DataSetProcessingException {
        String signature = CLASS_NAME
                + ".searchEntities(Class<?> ownerClass, String searchText, Page page, Restriction restriction)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "entityClass", "searchText", "page",
                "restriction" }, new Object[] { entityClass, searchText, page, restriction });

        try {
            PagedResults<EntityInfo> result = new PagedResults<EntityInfo>();

            if (restriction == null) {
                String tableName = ENTITY_TABLE_NAME.get(entityClass);
                String sql = String.format("select id, name from %s where name like ?", tableName);
                String countSql = createCountSql(sql, tableName);
                sql = limitSqlResults(sql, page);

                String pattern = "%" + searchText + "%";
                List<EntityInfo> entitiesInfo = jdbcTemplate.query(sql, new EntityInfoRowMapper(), pattern);
                long total = jdbcTemplate.queryForLong(countSql, pattern);

                result.setResults(entitiesInfo);
                result.setTotal(total);
            } else {
                if (entityClass == TargetType.class) {
                    throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                            "Restriction can not be applied to TargetType (TargetType is top of the hierarchy)"));
                } else if (entityClass == Target.class) {
                    result = getTargetsInfoInternal(searchText, signature, page, restriction);
                } else if (entityClass == Mission.class) {
                    result = getMissionsInfoInternal(searchText, signature, page, restriction);
                } else if (entityClass == Instrument.class) {
                    result = getInstrumentsInfoInternal(searchText, signature, page, restriction);
                } else if (entityClass == DataSet.class) {
                    result = getDataSetsInfoInternal(searchText, signature, page, restriction);
                } else if (entityClass == DataFile.class) {
                    result = getDataFilesInfoInternal(searchText, signature, page, restriction);
                }
            }

            LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });
            return result;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to execute search request", e));
        }
    }

    /**
     * <p>
     * Search data set by criteria.
     * </p>
     * <p>
     * Update: Add various filter criteria for newly added fields of SearchCriteria.
     * </p>
     */
    @Override
    public PagedResults<EntityInfo> searchDataSetsByCriteria(SearchCriteria criteria, Page page)
            throws DataSetProcessingException {
        String signature = CLASS_NAME + ".searchDataSetsByCriteria(SearchCriteria criteria, Page page)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "criteria", "page" }, new Object[] {
                criteria, page });

        try {
            WhereSqlAndArgs whereSqlAndArgs = createWhereSqlAndArgsForSearchCriteria(criteria);
            StringBuilder innerJoinSql = createInnerJoinSqlForSearchCriteria(criteria, whereSqlAndArgs);
            StringBuilder sql = createSearchByCriteriaSql("select distinct(DS.id), DS.name from dataset DS",
                    whereSqlAndArgs, innerJoinSql);

            PagedResults<EntityInfo> result = getEntitiesInfo(signature, page, "DS", sql.toString(),
                    whereSqlAndArgs.getArgs());
            LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });

            return result;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to search data sets", e));
        }
    }

    /**
     * <p>
     * Search for MapImages that match the SearchCriteria
     * </p>
     */
    @Override
    public PagedResults<EntityInfo> searchMapImagesByCriteria(SearchCriteria criteria, Page page)
            throws DataSetProcessingException {
        String signature = CLASS_NAME + ".searchMapImagesByCriteria(SearchCriteria criteria, Page page)";

        LoggingWrapperUtility.logEntrance(logger, signature, new String[] { "criteria", "page" }, new Object[] {
                criteria, page });

        try {
            WhereSqlAndArgs whereSqlAndArgs = createWhereSqlAndArgsForSearchCriteria(criteria);
            StringBuilder innerJoinSql = createInnerJoinSqlForSearchCriteria(criteria, whereSqlAndArgs);
            StringBuilder sql = createSearchByCriteriaSql("select distinct(MI.id), MI.name from dataset DS",
                    whereSqlAndArgs, innerJoinSql);

            PagedResults<EntityInfo> result = getEntitiesInfo(signature, page, "MI", sql.toString(),
                    whereSqlAndArgs.getArgs());
            LoggingWrapperUtility.logExit(logger, signature, new Object[] { result });

            return result;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to search data sets", e));
        }
    }

    @Override
    public List<String> getMapImagePaths(List<EntityInfo> mapImageEntityInfos) {
        // gather all the ids we will be querying for, by extracting them from the EntityInfos
        Queue<Long> allIds = new LinkedList<Long>();

        for (EntityInfo ei : mapImageEntityInfos) {
            allIds.add(ei.getId());
        }

        // create the collection to hold the paths we'll return
        List<String> paths = new ArrayList<String>();

        // while we still have id's to look up...
        while (!allIds.isEmpty()) {
            // ...collect 10 id's that we'll use in the prepared statement this go around (there are ten placeholders in
            // the SQL - arbitrary number).
            List<Long> thisIterationIds = new ArrayList<Long>();

            for (int i = 0; i < 10; i++) {
                // fetch the next id
                Long id = allIds.poll();

                if (id == null) {
                    // if no more ids, pad the in clause with nulls
                    thisIterationIds.add(null);
                } else {
                    thisIterationIds.add(id);
                }
            }

            // query + add
            paths.addAll(jdbcTemplate.queryForList(GET_MAP_IMAGE_PATH_SQL, String.class,
                    thisIterationIds.toArray(new Object[0])));
        }

        return paths;
    }

    // =========================================================================

    /**
     * Called by the methods that search the database for entities matching a {@link SearchCriteria} . This method
     * creates a single statement comprising of the leadInSql + innerJoinSql + whereSql.
     */
    private StringBuilder createSearchByCriteriaSql(String leadInSql, WhereSqlAndArgs whereSqlAndArgs,
            StringBuilder innerJoinSql) {
        StringBuilder whereSql = whereSqlAndArgs.getWhereSql();

        StringBuilder sql = new StringBuilder(leadInSql);

        if (!innerJoinSql.toString().trim().isEmpty()) {
            sql.append(innerJoinSql);
        }

        if (!whereSql.toString().trim().isEmpty()) {
            sql.append(" where ").append(whereSql.substring(4));
        }

        return sql;
    }

    /**
     * Mining for entities that match a SearchCriteria is a tricky business. This method is responsible for creating the
     * "INNER JOIN" clause(s) required for matching, based on the criteria parameters that are set.
     */
    private StringBuilder createInnerJoinSqlForSearchCriteria(SearchCriteria criteria, WhereSqlAndArgs whereSqlAndArgs) {
        StringBuilder whereSql = whereSqlAndArgs.getWhereSql();
        List<Object> args = whereSqlAndArgs.getArgs();

        List<String> joinTableNames = new ArrayList<String>();
        processEntitySearchStrings(criteria.getTargetTypes(), "TT", whereSql, args, joinTableNames);
        processEntitySearchStrings(criteria.getTargets(), "T", whereSql, args, joinTableNames);
        processEntitySearchStrings(criteria.getMissions(), "M", whereSql, args, joinTableNames);
        processEntitySearchStrings(criteria.getInstrumentHosts(), "IH", whereSql, args, joinTableNames);
        processEntitySearchStrings(criteria.getInstruments(), "I", whereSql, args, joinTableNames);

        StringBuilder innerJoinSql = new StringBuilder();
        if (joinTableNames.contains("T") && joinTableNames.contains("TT")) {
            joinTableNames.remove("T");
        }
        if (joinTableNames.contains("I") && joinTableNames.contains("IH")) {
            joinTableNames.remove("I");
        }

        for (String joinTableName : joinTableNames) {
            if (joinTableName.equals("TT")) {
                innerJoinSql.append(" inner join dataset_target DT              on DS.id = DT.dataset_id")
                        .append(" inner join target T                       on DT.target_id = T.id")
                        .append(" inner join target_type_target TTT         on T.id = TTT.target_id")
                        .append(" inner join target_type TT                 on TTT.target_type_id = TT.id");
            }
            if (joinTableName.equals("T")) {
                innerJoinSql.append(" inner join dataset_target DT              on DS.id = DT.dataset_id").append(
                        " inner join target T                       on DT.target_id = T.id");
            }
            if (joinTableName.equals("M")) {
                innerJoinSql.append(" inner join dataset_mission DM             on DS.id = DM.dataset_id").append(
                        " inner join mission M                      on DM.mission_id = M.id");
            }
            if (joinTableName.equals("IH")) {
                innerJoinSql.append(" inner join instrument_catalog IC          on DS.id = IC.dataset_id")
                        .append(" inner join instrument I                   on IC.instrument_id = I.id")
                        .append(" inner join instrument_host_instrument IHI on I.id = IHI.instrument_id")
                        .append(" inner join instrument_host IH             on IHI.instrument_host_id = IH.id");
            }
            if (joinTableName.equals("I")) {
                innerJoinSql.append(" inner join instrument_catalog IC          on DS.id = IC.dataset_id").append(
                        " inner join instrument I                   on IC.instrument_id = I.id");
            }
            if (joinTableName.equals("rating")) {
                innerJoinSql.append(" inner join dataset_rating DR              on DS.id = DR.dataset_id");
            }
        }
        if (criteria.isUseLRO()) {
            innerJoinSql.append(" inner join dataset_map_image DMI on DS.id = DMI.dataset_id").append(
                    " inner join map_image MI on DMI.map_image_id = MI.id");
        }
        return innerJoinSql;
    }

    /**
     * This method is responsible for creating the "WHERE" portion of an SQL statement to be used in conjunction with
     * searching for entities that match a {@link SearchCriteria}
     */
    private WhereSqlAndArgs createWhereSqlAndArgsForSearchCriteria(SearchCriteria criteria) {
        WhereSqlAndArgs whereSqlAndArgs = new WhereSqlAndArgs();

        if (criteria.getDataSetId() != 0) {
            whereSqlAndArgs.append(" and DS.id = ?", criteria.getDataSetId());
        }
        if (criteria.getStartDate() != null) {
            whereSqlAndArgs.append(" and DS.start_time >= ?", criteria.getStartDate());
        }
        if (criteria.getStopDate() != null) {
            whereSqlAndArgs.append(" and DS.stop_time <= ?", criteria.getStopDate());
        }

        if (criteria.isUseLRO()) {
            if (criteria.getLongitudeMin() != null) {
                whereSqlAndArgs.append(" and MI.center_longitude >= ?", criteria.getLongitudeMin());
            }
            if (criteria.getLongitudeMax() != null) {
                whereSqlAndArgs.append(" and  MI.center_longitude <= ?", criteria.getLongitudeMax());
            }
            if (criteria.getLatitudeMin() != null) {
                whereSqlAndArgs.append(" and MI.center_latitude >= ?", criteria.getLatitudeMin());
            }
            if (criteria.getLatitudeMax() != null) {
                whereSqlAndArgs.append(" and  MI.center_latitude <= ?", criteria.getLatitudeMax());
            }
            if (criteria.getIlluminationMin() != null) {
                whereSqlAndArgs.append(" and MI.illumination >= ?", criteria.getIlluminationMin());
            }
            if (criteria.getIlluminationMax() != null) {
                whereSqlAndArgs.append(" and  MI.illumination <= ?", criteria.getIlluminationMax());
            }
            if (criteria.getCameraAngleMin() != null) {
                whereSqlAndArgs.append(" and MI.camera_angle >= ?", criteria.getCameraAngleMin());
            }
            if (criteria.getCameraAngleMax() != null) {
                whereSqlAndArgs.append(" and  MI.camera_angle <= ?", criteria.getCameraAngleMax());
            }
            if (criteria.getProductType() != null) {
                whereSqlAndArgs.append(" and MI.product_type = ?", criteria.getProductType());
            }
            if (criteria.getCameraSpecification() != null) {
                whereSqlAndArgs.append(" and MI.camera_spec = ?", criteria.getCameraSpecification());
            }
            if (criteria.getCollectionDateMin() != null) {
                whereSqlAndArgs.append(" and MI.date >= ?", criteria.getCollectionDateMin());
            }
            if (criteria.getCollectionDateMax() != null) {
                whereSqlAndArgs.append(" and MI.date <= ?", criteria.getCollectionDateMax());
            }
        }
        return whereSqlAndArgs;
    }

    /**
     * Updates 'whereSql' string with search patterns.
     *
     * @param strs
     *            the search patterns
     * @param tableName
     *            the name of the entity's table
     * @param whereSql
     *            the where sql string
     * @param args
     *            the request parameters
     * @param joinTableNames
     *            the join table names list that is updated by this method
     */
    private void processEntitySearchStrings(List<String> strs, String tableName, StringBuilder whereSql,
            List<Object> args, List<String> joinTableNames) {
        if (strs == null || strs.isEmpty()) {
            return;
        }
        boolean first = true;
        for (String str : strs) {
            whereSql.append(first ? " and (" : " or ");
            whereSql.append(tableName + ".name like ?");
            args.add("%" + str + "%");
            first = false;
        }
        whereSql.append(")");
        joinTableNames.add(tableName);
    }

    // =========================================================================

    /**
     * Row mapper that creates EntityInfo instances with basic entity info: id and name.
     */
    private static class EntityInfoRowMapper implements RowMapper<EntityInfo> {
        @Override
        public EntityInfo mapRow(ResultSet rs, int rowNum) throws SQLException {
            EntityInfo entityInfo = new EntityInfo();
            entityInfo.setId(rs.getLong("id"));
            entityInfo.setName(rs.getString("name"));
            return entityInfo;
        }
    }

    /**
     * Retrieves info (id and name) about entities of the given type for the given page (or for all pages if page is
     * null).
     */
    private PagedResults<EntityInfo> getEntitiesInfo(String signature, Page page, String tableName, String sql,
            List<Object> args) throws DataSetProcessingException {
        try {
            String countSql = createCountSql(sql, tableName);
            sql = limitSqlResults(sql, page);

            Object[] argsArray = (args == null) ? null : args.toArray();
            List<EntityInfo> entitiesInfo = jdbcTemplate.query(sql, new EntityInfoRowMapper(), argsArray);
            long total = jdbcTemplate.queryForLong(countSql, argsArray);

            PagedResults<EntityInfo> result = new PagedResults<EntityInfo>();
            result.setResults(entitiesInfo);
            result.setTotal(total);
            return result;
        } catch (DataAccessException e) {
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Failed to get entity info", e));
        } catch (DataSetProcessingException e) {
            throw LoggingWrapperUtility.logException(logger, signature, e);
        }
    }

    private <T> T getEntity(Class<T> entityClass, long entityId) {
        String tableName = ENTITY_TABLE_NAME.get(entityClass);
        String sql = String.format("select * from %s where id = ?", tableName);

        @SuppressWarnings("unchecked")
        List<T> result = jdbcTemplate.query(sql, (RowMapper<T>) ROW_MAPPERS.get(entityClass), entityId);

        return result.isEmpty() ? null : result.get(0);
    }

    /**
     * Applies LIMIT instruction to the given sql string based on the values defined by the page parameter.
     *
     * @param sql
     *            the sql string
     * @param page
     *            defines the page parameters. Can be null.
     *
     * @return the modified sql string
     *
     * @throws DataSetProcessingException
     *             if pageInfo.pageNumber < 1 or pageInfo.itemsPerPage < 1
     */
    private String limitSqlResults(String sql, Page page) throws DataSetProcessingException {
        if (page != null) {
            int pageNumber = page.getPageNumber();
            int itemsPerPage = page.getItemsPerPage();

            if (pageNumber < 1 || itemsPerPage < 1) {
                throw new DataSetProcessingException("Invalid state of page instance");
            }

            long offset = (pageNumber - 1) * itemsPerPage;
            sql += String.format(" LIMIT %d, %d", offset, itemsPerPage);
        }
        return sql;
    }

    private String createCountSql(String sql, String tableName) {
        sql = sql.toLowerCase();
        int start = sql.indexOf("select");
        if (start == -1) {
            return null;
        }
        start += 7;

        int end = sql.indexOf("from");
        if (end == -1) {
            return null;
        }
        end -= 1;

        String replaceStr = String.format("count(distinct %s.id)", tableName.toLowerCase());
        return (new StringBuilder(sql).replace(start, end, replaceStr)).toString();
    }

    private void checkRestriction(String signature, Restriction restriction,
            Class<?>... allowedRestrictiveEntityClasses) throws DataSetProcessingException {
        if (restriction != null) {
            for (Class<?> entityClass : allowedRestrictiveEntityClasses) {
                if (entityClass == restriction.getRestrictionEntityClass()) {
                    return;
                }
            }
            throw LoggingWrapperUtility.logException(logger, signature, new DataSetProcessingException(
                    "Invalid restriction entity type"));
        }
    }

    @Override
    public List<String> getAllProductTypes() {
        return jdbcTemplate.query(GET_ALL_PROCUCT_TYPES, new RowMapper<String>(){
            public String mapRow(ResultSet rs, int arg1) throws SQLException {
                return rs.getString(1);
            }});
    }
    
    @Override
    public List<String> getAllCameraSpecs() {
        return jdbcTemplate.query(GET_ALL_CAMERA_SPECS, new RowMapper<String>(){
            public String mapRow(ResultSet rs, int arg1) throws SQLException {
                return rs.getString(1);
            }});
    }
}
