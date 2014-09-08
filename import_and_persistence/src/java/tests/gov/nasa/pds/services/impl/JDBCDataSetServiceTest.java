/*
 * Copyright (C) 2014 TopCoder Inc., All Rights Reserved.
 */
package gov.nasa.pds.services.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import gov.nasa.pds.entities.CameraSpecification;
import gov.nasa.pds.entities.CameraType;
import gov.nasa.pds.entities.DataSet;
import gov.nasa.pds.entities.EntityInfo;
import gov.nasa.pds.entities.Instrument;
import gov.nasa.pds.entities.MapImage;
import gov.nasa.pds.entities.MetadataObject;
import gov.nasa.pds.entities.Mission;
import gov.nasa.pds.entities.PagedResults;
import gov.nasa.pds.entities.ProductType;
import gov.nasa.pds.entities.Reference;
import gov.nasa.pds.entities.SearchCriteria;
import gov.nasa.pds.entities.Target;
import gov.nasa.pds.entities.Volume;
import gov.nasa.pds.services.ConversionPersistence;
import gov.nasa.pds.services.DataSetService;

import java.sql.Connection;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import junit.framework.JUnit4TestAdapter;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * <p>
 * The unit test of {@link JDBCDataSetService} class. Only the methods changed in
 * assembly [NASA LMMP - PDS API Update Module Assembly] were tested.
 * </p>
 *
 * @author caoweiquan322
 * @version 1.0
 */
public class JDBCDataSetServiceTest {
    /**
     * The sql connection for test.
     */
    private Connection conn;

    /**
     * The application context for test.
     */
    private ApplicationContext context;

    /**
     * The conversion persistence for test.
     */
    private ConversionPersistence conversionPersistence;

    /**
     * The data dictionary import persistence for test.
     */
    private DataSetService dataSetService;

    /**
     * Adapter for JUnit.
     *
     * @return a test suite.
     */
    public static junit.framework.Test suite() {
        return new JUnit4TestAdapter(JDBCDataSetServiceTest.class);
    }

    /**
     * Sets up the unit tests.
     *
     * @throws Exception to JUnit.
     */
    @Before
    public void setUp() throws Exception {
        conn = TestHelper.getConnection();
        context = new ClassPathXmlApplicationContext("beans.xml");
        conversionPersistence = (ConversionPersistence) context.getBean("conversionPersistence");
        dataSetService = (DataSetService) context.getBean("dataSetService");

        TestHelper.execute(conn, "DELETE FROM `table_counter`");
        TestHelper.execute(conn, "INSERT INTO `table_counter` VALUES (1,0)");
    }

    /**
     * Tears down the test environment.
     *
     * @throws Exception to JUnit.
     */
    @After
    public void tearDown() throws Exception {
        try {
            TestHelper.deleteData(conn);
            TestHelper.execute(conn, "drop table IF EXISTS dummyTable");
        } finally {
            TestHelper.closeConnection(conn);
        }
    }

    /**
     * Test method <code>getDataSet(long dataSetId)</code>.
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testGetDataSet() throws Exception {
        // Prepare the DataSet object in mysql db.
        long dataSetId = insertDataset();

        // Retrieve dataset and check it.
        DataSet resultDataSet = dataSetService.getDataSet(dataSetId);
        List<Mission> missions = resultDataSet.getMissions();
        List<MapImage> mapImages = resultDataSet.getMapImages();
        assertEquals("Should stored 2 missions.", 2,
                missions.size());
        assertEquals("Should stored 1 map image.", 1,
                mapImages.size());
        MapImage mapImage = mapImages.get(0);
        DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
        double epsonal = 1e-5;
        // Check fields of the MapImage is correctly queried.
        // Do not check ID since we can not predict what the ID would be
        assertEquals("'image_name' should match.", "image name", mapImage.getName());
        assertEquals("'mission_id' should be correct", missions.get(1).getId(),
                mapImage.getMissionId());
        assertEquals("'image_path' should match.", "image path", mapImage.getImagePath());
        assertEquals("'date' should be correct", formatter.format(new Date()),
                formatter.format(mapImage.getDate()));
        assertTrue("'center_longitude' should be correct",
                Math.abs(mapImage.getCenterLongitude() - 0.0)
                        < epsonal);
        assertTrue("'center_latitude' should be correct",
                Math.abs(mapImage.getCenterLongitude() - 0.0)
                        < epsonal);
        assertTrue("'illumination' should be correct",
                Math.abs(mapImage.getCenterLongitude() - 0.0)
                        < epsonal);
        assertTrue("'camera_angle' should be correct",
                Math.abs(mapImage.getCenterLongitude() - 0.0)
                        < epsonal);
        assertEquals("'camera_type' should be correct", "LROC_LEFT",
                mapImage.getCameraType().toString());
    }

    /**
     * Test method <code>searchDataSetsByCriteria(SearchCriteria criteria, Page page)</code>.
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testSearchDataSetsByCriteria1() throws Exception {
        // Prepare the DataSet object in mysql db.
        long dataSetId = insertDataset();

        // Prepare search criteria
        // We only care about the map image related criterias, so leave
        // other search fields to null.
        SearchCriteria criteria = new SearchCriteria();
        criteria.setUseLRO(true);
        criteria.setLongitudeMin(-100.0);
        criteria.setLongitudeMax(100.0);
        criteria.setLatitudeMin(-100.0);
        criteria.setLatitudeMax(100.0);
        criteria.setIlluminationMin(-0.1);
        criteria.setIlluminationMax(1.0);
        criteria.setCameraAngleMin(-100.0);
        criteria.setCameraAngleMax(100.0);

        // Retrieve dataset and check it.
        PagedResults<EntityInfo> info = dataSetService.searchDataSetsByCriteria(criteria, null);
        assertEquals("Should stored only 1 dataset.", 1,
                info.getTotal());
        assertEquals("The dataset id should match since there's only one DataSet in the database.",
                dataSetId,
                info.getResults().get(0).getId());
    }

    /**
     * Test method <code>searchDataSetsByCriteria(SearchCriteria criteria, Page page)</code>.
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testSearchDataSetsByCriteria2() throws Exception {
        // Prepare the DataSet object in mysql db.
        insertDataset();

        // Prepare search criteria
        // We only care about the map image related criterias, so leave
        // other search fields to null.
        SearchCriteria criteria = new SearchCriteria();
        criteria.setUseLRO(true);
        // Set the longitude to [50, 100], so that no dataset would be searched.
        criteria.setLongitudeMin(50.0);
        criteria.setLongitudeMax(100.0);
        criteria.setLatitudeMin(-100.0);
        criteria.setLatitudeMax(100.0);
        criteria.setIlluminationMin(-0.1);
        criteria.setIlluminationMax(1.0);
        criteria.setCameraAngleMin(-100.0);
        criteria.setCameraAngleMax(100.0);

        // Retrieve dataset and check it.
        PagedResults<EntityInfo> info = dataSetService.searchDataSetsByCriteria(criteria, null);
        assertEquals("Should found 0 dataset.", 0,
                info.getTotal());
    }

    /**
     * Test method <code>searchDataSetsByCriteria(SearchCriteria criteria, Page page)</code>.
     *
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testSearchDataSetsByCriteria3() throws Exception {
        // Prepare search criteria
        // We only care about the map image related criterias, so leave
        // other search fields to null.
        SearchCriteria criteria = new SearchCriteria();
        criteria.setUseLRO(true);
        criteria.setLongitudeMin(-100.0);
        testMultiRecordsSingleCondition(criteria);
    }

    /**
     * Test method <code>searchDataSetsByCriteria(SearchCriteria criteria, Page page)</code>.
     *
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testSearchDataSetsByCriteria4() throws Exception {
        // Prepare search criteria
        // We only care about the map image related criterias, so leave
        // other search fields to null.
        SearchCriteria criteria = new SearchCriteria();
        criteria.setUseLRO(true);
        criteria.setLongitudeMax(100.0);
        testMultiRecordsSingleCondition(criteria);
    }

    /**
     * Test method <code>searchDataSetsByCriteria(SearchCriteria criteria, Page page)</code>.
     *
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testSearchDataSetsByCriteria5() throws Exception {
        // Prepare search criteria
        // We only care about the map image related criterias, so leave
        // other search fields to null.
        SearchCriteria criteria = new SearchCriteria();
        criteria.setUseLRO(true);
        criteria.setLatitudeMin(-100.0);
        testMultiRecordsSingleCondition(criteria);
    }

    /**
     * Test method <code>searchDataSetsByCriteria(SearchCriteria criteria, Page page)</code>.
     *
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testSearchDataSetsByCriteria6() throws Exception {
        // Prepare search criteria
        // We only care about the map image related criterias, so leave
        // other search fields to null.
        SearchCriteria criteria = new SearchCriteria();
        criteria.setUseLRO(true);
        criteria.setLatitudeMax(100.0);
        testMultiRecordsSingleCondition(criteria);
    }

    /**
     * Test method <code>searchDataSetsByCriteria(SearchCriteria criteria, Page page)</code>.
     *
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testSearchDataSetsByCriteria7() throws Exception {
        // Prepare search criteria
        // We only care about the map image related criterias, so leave
        // other search fields to null.
        SearchCriteria criteria = new SearchCriteria();
        criteria.setUseLRO(true);
        criteria.setIlluminationMin(-100.0);
        testMultiRecordsSingleCondition(criteria);
    }

    /**
     * Test method <code>searchDataSetsByCriteria(SearchCriteria criteria, Page page)</code>.
     *
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testSearchDataSetsByCriteria8() throws Exception {
        // Prepare search criteria
        // We only care about the map image related criterias, so leave
        // other search fields to null.
        SearchCriteria criteria = new SearchCriteria();
        criteria.setUseLRO(true);
        criteria.setIlluminationMax(100.0);
        testMultiRecordsSingleCondition(criteria);
    }

    /**
     * Test method <code>searchDataSetsByCriteria(SearchCriteria criteria, Page page)</code>.
     *
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testSearchDataSetsByCriteria9() throws Exception {
        // Prepare search criteria
        // We only care about the map image related criterias, so leave
        // other search fields to null.
        SearchCriteria criteria = new SearchCriteria();
        criteria.setUseLRO(true);
        criteria.setCameraAngleMin(-100.0);
        testMultiRecordsSingleCondition(criteria);
    }

    /**
     * Test method <code>searchDataSetsByCriteria(SearchCriteria criteria, Page page)</code>.
     *
     *
     * @throws Exception to JUnit.
     */
    @Test
    public void testSearchDataSetsByCriteria10() throws Exception {
        // Prepare search criteria
        // We only care about the map image related criterias, so leave
        // other search fields to null.
        SearchCriteria criteria = new SearchCriteria();
        criteria.setUseLRO(true);
        criteria.setCameraAngleMax(100.0);
        testMultiRecordsSingleCondition(criteria);
    }

    /**
     * Helper method to test single condition search for several records.
     * We assume the criteria will filter out all 2 records.
     *
     * @param criteria
     *             the {@link SearchCriteria} object to filter records
     *
     * @throws Exception to JUnit.
     */
    private void testMultiRecordsSingleCondition(SearchCriteria criteria) throws Exception {
        // Prepare the DataSet object in mysql db.
        long dataSetId1 = insertDataset();
        long dataSetId2 = insertDataset();

        // Retrieve dataset and check it.
        PagedResults<EntityInfo> info = dataSetService.searchDataSetsByCriteria(criteria, null);
        assertEquals("Should stored only 2 dataset.", 2,
                info.getTotal());
        assertTrue("The dataset id should match.",
                (dataSetId1 == info.getResults().get(0).getId()
                && dataSetId2 == info.getResults().get(1).getId())
                || (dataSetId2 == info.getResults().get(0).getId()
                && dataSetId1 == info.getResults().get(1).getId()));
    }

    /**
     * Helper method to insert one {@link DataSet} object to the database.
     *
     * @return
     *             id of the {@link DataSet}
     * @throws Exception
     *             to JUnit
     */
    private long insertDataset() throws Exception {
        DataSet dataSet = new DataSet();
        dataSet.setDescription("description");
        dataSet.setName("name");
        dataSet.setRating(1.1f);
        dataSet.setStartDate(new Timestamp(System.currentTimeMillis()));
        dataSet.setStopDate(new Timestamp(System.currentTimeMillis()));
        dataSet.setTextId("textId");

        // references
        dataSet.setReferences(new ArrayList<Reference>());
        Reference reference = new Reference();
        reference.setDescription("description1");
        reference.setKeyTextId("keyTextId1");
        long[] referenceIds = new long[2];
        referenceIds[0] = conversionPersistence.insertReference(reference);
        reference.setId(referenceIds[0]);
        dataSet.getReferences().add(reference);
        reference = new Reference();
        reference.setDescription("description2");
        reference.setKeyTextId("keyTextId2");
        referenceIds[1] = conversionPersistence.insertReference(reference);
        reference.setId(referenceIds[1]);
        dataSet.getReferences().add(reference);

        // other children
        dataSet.setOtherChildren(new ArrayList<MetadataObject>());
        dataSet.getOtherChildren().add(TestHelper.createMetadataObject("name1"));
        dataSet.getOtherChildren().get(0).getProperties().add(TestHelper.createProperty("name2", "value1", "value2"));
        dataSet.getOtherChildren().get(0).setChildren(new ArrayList<MetadataObject>());
        dataSet.getOtherChildren().get(0).getChildren().add(TestHelper.createMetadataObject("name3"));
        dataSet.getOtherChildren().get(0).getChildren().get(0).getProperties()
            .add(TestHelper.createProperty("name4", "value3", "value4"));
        dataSet.getOtherChildren().get(0).getChildren().get(0).getProperties()
            .add(TestHelper.createProperty("name5", "value5", "value6"));

        // as insertInstrument(Instrument instrument), insertMissions(Mission mission).
        // insertTarget(Target target) and  insertTarget(Target target) are called
        // so only check the records in instrument and dataset_instrument tables, ....
        dataSet.setInstruments(new ArrayList<Instrument>());
        Instrument instrument = new Instrument();
        instrument.setDescription("description1");
        instrument.setName("name1");
        instrument.setTextId("textId1");
        instrument.setType("type1");
        conversionPersistence.insertInstrument(instrument);
        dataSet.getInstruments().add(instrument);
        instrument = new Instrument();
        instrument.setDescription("description2");
        instrument.setName("name2");
        instrument.setTextId("textId2");
        instrument.setType("type2");
        conversionPersistence.insertInstrument(instrument);
        dataSet.getInstruments().add(instrument);

        dataSet.setMissions(new ArrayList<Mission>());
        Mission mission = new Mission();
        mission.setDescription("description1");
        mission.setEndDate(new Timestamp(System.currentTimeMillis()));
        mission.setName("name1");
        mission.setStartDate(new Timestamp(System.currentTimeMillis()));
        conversionPersistence.insertMission(mission);
        dataSet.getMissions().add(mission);
        mission = new Mission();
        mission.setDescription("description2");
        mission.setEndDate(new Timestamp(System.currentTimeMillis()));
        mission.setName("name2");
        mission.setStartDate(new Timestamp(System.currentTimeMillis()));
        conversionPersistence.insertMission(mission);
        dataSet.getMissions().add(mission);

        dataSet.setMapImages(new ArrayList<MapImage>());
        MapImage mapImage = new MapImage();
        mapImage.setName("image name");
        mapImage.setMissionId(mission.getId());
        mapImage.setImagePath("image path");
        mapImage.setDate(new Date());
        mapImage.setCenterLongitude(0.0);
        mapImage.setCenterLatitude(0.0);
        mapImage.setIllumination(0.0);
        mapImage.setCameraAngle(0.0);
        mapImage.setCameraType(CameraType.LROC_LEFT);
        mapImage.setProductType(ProductType.CDRNAC);
        mapImage.setCameraSpecification(CameraSpecification.NAC);
        conversionPersistence.insertMapImage(mapImage);
        dataSet.getMapImages().add(mapImage);

        dataSet.setTargets(new ArrayList<Target>());
        Target target = new Target();
        target.setName("name1");
        conversionPersistence.insertTarget(target);
        dataSet.getTargets().add(target);
        target = new Target();
        target.setName("name2");
        conversionPersistence.insertTarget(target);
        dataSet.getTargets().add(target);

        dataSet.setVolumes(new ArrayList<Volume>());
        Volume volume = new Volume();
        volume.setName("name1");
        volume.setDescription("description1");
        volume.setTextId("textId1");
        volume.setSetTextId("setTextId1");
        volume.setSetName("setName1");
        volume.setSeriesName("seriesName1");
        conversionPersistence.insertVolume(volume);
        dataSet.getVolumes().add(volume);
        volume = new Volume();
        volume.setName("name2");
        volume.setDescription("description2");
        volume.setTextId("textId2");
        volume.setSetTextId("setTextId2");
        volume.setSetName("setName2");
        volume.setSeriesName("seriesName2");
        conversionPersistence.insertVolume(volume);
        dataSet.getVolumes().add(volume);

        long dataSetId = conversionPersistence.insertDataSet(dataSet);
        conversionPersistence.clearCaches();
        return dataSetId;
    }
}
