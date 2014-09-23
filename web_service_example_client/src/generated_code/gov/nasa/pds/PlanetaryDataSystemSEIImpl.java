
/**
 * Please modify this class to meet your needs
 * This class is not complete
 */

package gov.nasa.pds;

import java.util.logging.Logger;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;

/**
 * This class was generated by Apache CXF 2.5.2
 * 2012-05-14T17:08:08.123+03:00
 * Generated source version: 2.5.2
 * 
 */

@javax.jws.WebService(
                      serviceName = "PlanetaryDataSystemService",
                      portName = "PlanetaryDataSystemPort",
                      targetNamespace = "http://pds.nasa.gov/",
                      wsdlLocation = "http://50.19.174.233:8080/nasa_pds_ws/services/PlanetaryDataSystemPort?wsdl",
                      endpointInterface = "gov.nasa.pds.PlanetaryDataSystemSEI")
                      
public class PlanetaryDataSystemSEIImpl implements PlanetaryDataSystemSEI {

    private static final Logger LOG = Logger.getLogger(PlanetaryDataSystemSEIImpl.class.getName());

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getTargetsInfo(gov.nasa.pds.Page  page ,)gov.nasa.pds.Restriction  restriction )*
     */
    public gov.nasa.pds.PagedResults getTargetsInfo(gov.nasa.pds.Page page,gov.nasa.pds.Restriction restriction) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getTargetsInfo");
        System.out.println(page);
        System.out.println(restriction);
        try {
            gov.nasa.pds.PagedResults _return = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnResultsVal1 = null;
            _returnResults.add(_returnResultsVal1);
            _return.getResults().addAll(_returnResults);
            _return.setTotal(144253195600777978l);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getPreviewImageURL(long  imageFileId )*
     */
    public java.lang.String getPreviewImageURL(long imageFileId) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getPreviewImageURL");
        System.out.println(imageFileId);
        try {
            java.lang.String _return = "_return-752903142";
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getInstrument(long  id )*
     */
    public gov.nasa.pds.Instrument getInstrument(long id) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getInstrument");
        System.out.println(id);
        try {
            gov.nasa.pds.Instrument _return = new gov.nasa.pds.Instrument();
            _return.setId(-8189202376477283335l);
            _return.setName("Name-1973762739");
            _return.setDescription("Description1175456930");
            java.util.List<gov.nasa.pds.InstrumentHost> _returnHosts = new java.util.ArrayList<gov.nasa.pds.InstrumentHost>();
            gov.nasa.pds.InstrumentHost _returnHostsVal1 = new gov.nasa.pds.InstrumentHost();
            _returnHostsVal1.setId(-9189978408071031626l);
            _returnHostsVal1.setName("Name1054700694");
            java.util.List<gov.nasa.pds.MetadataObject> _returnHostsVal1OtherChildren = new java.util.ArrayList<gov.nasa.pds.MetadataObject>();
            _returnHostsVal1.getOtherChildren().addAll(_returnHostsVal1OtherChildren);
            java.util.List<gov.nasa.pds.Reference> _returnHostsVal1References = new java.util.ArrayList<gov.nasa.pds.Reference>();
            _returnHostsVal1.getReferences().addAll(_returnHostsVal1References);
            _returnHostsVal1.setTextId("TextId-1784912553");
            _returnHosts.add(_returnHostsVal1);
            _return.getHosts().addAll(_returnHosts);
            java.util.List<gov.nasa.pds.MetadataObject> _returnOtherChildren = new java.util.ArrayList<gov.nasa.pds.MetadataObject>();
            gov.nasa.pds.MetadataObject _returnOtherChildrenVal1 = new gov.nasa.pds.MetadataObject();
            _returnOtherChildrenVal1.setId(-8833206658633842660l);
            _returnOtherChildrenVal1.setName("Name-167333747");
            java.util.List<gov.nasa.pds.MetadataObject> _returnOtherChildrenVal1Children = new java.util.ArrayList<gov.nasa.pds.MetadataObject>();
            _returnOtherChildrenVal1.getChildren().addAll(_returnOtherChildrenVal1Children);
            java.util.List<gov.nasa.pds.Property> _returnOtherChildrenVal1Properties = new java.util.ArrayList<gov.nasa.pds.Property>();
            _returnOtherChildrenVal1.getProperties().addAll(_returnOtherChildrenVal1Properties);
            _returnOtherChildren.add(_returnOtherChildrenVal1);
            _return.getOtherChildren().addAll(_returnOtherChildren);
            java.util.List<gov.nasa.pds.Reference> _returnReferences = new java.util.ArrayList<gov.nasa.pds.Reference>();
            gov.nasa.pds.Reference _returnReferencesVal1 = new gov.nasa.pds.Reference();
            _returnReferencesVal1.setId(-739394373903813735l);
            _returnReferencesVal1.setDescription("Description934893231");
            _returnReferencesVal1.setKeyTextId("KeyTextId1839997801");
            _returnReferences.add(_returnReferencesVal1);
            _return.getReferences().addAll(_returnReferences);
            _return.setTextId("TextId1875499077");
            _return.setType("Type1437472186");
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#searchDataSetsByCriteria(gov.nasa.pds.SearchCriteria  criteria ,)gov.nasa.pds.Page  page )*
     */
    public gov.nasa.pds.PagedResults searchDataSetsByCriteria(gov.nasa.pds.SearchCriteria criteria,gov.nasa.pds.Page page) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation searchDataSetsByCriteria");
        System.out.println(criteria);
        System.out.println(page);
        try {
            gov.nasa.pds.PagedResults _return = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnResultsVal1 = null;
            _returnResults.add(_returnResultsVal1);
            _return.getResults().addAll(_returnResults);
            _return.setTotal(-812204795537305655l);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getTarget(long  id )*
     */
    public gov.nasa.pds.Target getTarget(long id) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getTarget");
        System.out.println(id);
        try {
            gov.nasa.pds.Target _return = new gov.nasa.pds.Target();
            _return.setId(-3324203588018416691l);
            _return.setName("Name1109200152");
            java.util.List<gov.nasa.pds.Reference> _returnReferences = new java.util.ArrayList<gov.nasa.pds.Reference>();
            gov.nasa.pds.Reference _returnReferencesVal1 = new gov.nasa.pds.Reference();
            _returnReferencesVal1.setId(-3911929502756579808l);
            _returnReferencesVal1.setDescription("Description1601889563");
            _returnReferencesVal1.setKeyTextId("KeyTextId1311525244");
            _returnReferences.add(_returnReferencesVal1);
            _return.getReferences().addAll(_returnReferences);
            java.util.List<gov.nasa.pds.TargetType> _returnTypes = new java.util.ArrayList<gov.nasa.pds.TargetType>();
            gov.nasa.pds.TargetType _returnTypesVal1 = new gov.nasa.pds.TargetType();
            _returnTypesVal1.setId(4891411305530941211l);
            _returnTypesVal1.setName("Name-1514518748");
            _returnTypes.add(_returnTypesVal1);
            _return.getTypes().addAll(_returnTypes);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getTargetTypesInfo(gov.nasa.pds.Page  page )*
     */
    public gov.nasa.pds.PagedResults getTargetTypesInfo(gov.nasa.pds.Page page) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getTargetTypesInfo");
        System.out.println(page);
        try {
            gov.nasa.pds.PagedResults _return = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnResultsVal1 = null;
            _returnResults.add(_returnResultsVal1);
            _return.getResults().addAll(_returnResults);
            _return.setTotal(-3415610535148549908l);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getMission(long  id )*
     */
    public gov.nasa.pds.Mission getMission(long id) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getMission");
        System.out.println(id);
        try {
            gov.nasa.pds.Mission _return = new gov.nasa.pds.Mission();
            _return.setId(-1648159174714588956l);
            _return.setName("Name-280694761");
            _return.setDescription("Description-218889795");
            _return.setEndDate(javax.xml.datatype.DatatypeFactory.newInstance().newXMLGregorianCalendar("2012-05-14T17:08:08.133+03:00"));
            java.util.List<gov.nasa.pds.MetadataObject> _returnOtherChildren = new java.util.ArrayList<gov.nasa.pds.MetadataObject>();
            gov.nasa.pds.MetadataObject _returnOtherChildrenVal1 = new gov.nasa.pds.MetadataObject();
            _returnOtherChildrenVal1.setId(-5988480047654611368l);
            _returnOtherChildrenVal1.setName("Name106211820");
            java.util.List<gov.nasa.pds.MetadataObject> _returnOtherChildrenVal1Children = new java.util.ArrayList<gov.nasa.pds.MetadataObject>();
            _returnOtherChildrenVal1.getChildren().addAll(_returnOtherChildrenVal1Children);
            java.util.List<gov.nasa.pds.Property> _returnOtherChildrenVal1Properties = new java.util.ArrayList<gov.nasa.pds.Property>();
            _returnOtherChildrenVal1.getProperties().addAll(_returnOtherChildrenVal1Properties);
            _returnOtherChildren.add(_returnOtherChildrenVal1);
            _return.getOtherChildren().addAll(_returnOtherChildren);
            java.util.List<gov.nasa.pds.Reference> _returnReferences = new java.util.ArrayList<gov.nasa.pds.Reference>();
            gov.nasa.pds.Reference _returnReferencesVal1 = new gov.nasa.pds.Reference();
            _returnReferencesVal1.setId(-6511515848021299922l);
            _returnReferencesVal1.setDescription("Description-1204998081");
            _returnReferencesVal1.setKeyTextId("KeyTextId674527226");
            _returnReferences.add(_returnReferencesVal1);
            _return.getReferences().addAll(_returnReferences);
            _return.setStartDate(javax.xml.datatype.DatatypeFactory.newInstance().newXMLGregorianCalendar("2012-05-14T17:08:08.133+03:00"));
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getMissionsInfo(gov.nasa.pds.Page  page ,)gov.nasa.pds.Restriction  restriction )*
     */
    public gov.nasa.pds.PagedResults getMissionsInfo(gov.nasa.pds.Page page,gov.nasa.pds.Restriction restriction) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getMissionsInfo");
        System.out.println(page);
        System.out.println(restriction);
        try {
            gov.nasa.pds.PagedResults _return = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnResultsVal1 = null;
            _returnResults.add(_returnResultsVal1);
            _return.getResults().addAll(_returnResults);
            _return.setTotal(2797398302611797629l);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getDocumentsInfo(gov.nasa.pds.Page  page ,)gov.nasa.pds.Restriction  restriction )*
     */
    public gov.nasa.pds.PagedResults getDocumentsInfo(gov.nasa.pds.Page page,gov.nasa.pds.Restriction restriction) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getDocumentsInfo");
        System.out.println(page);
        System.out.println(restriction);
        try {
            gov.nasa.pds.PagedResults _return = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnResultsVal1 = null;
            _returnResults.add(_returnResultsVal1);
            _return.getResults().addAll(_returnResults);
            _return.setTotal(1836408878985250608l);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getInstrumentsInfo(gov.nasa.pds.Page  page ,)gov.nasa.pds.Restriction  restriction )*
     */
    public gov.nasa.pds.PagedResults getInstrumentsInfo(gov.nasa.pds.Page page,gov.nasa.pds.Restriction restriction) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getInstrumentsInfo");
        System.out.println(page);
        System.out.println(restriction);
        try {
            gov.nasa.pds.PagedResults _return = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnResultsVal1 = null;
            _returnResults.add(_returnResultsVal1);
            _return.getResults().addAll(_returnResults);
            _return.setTotal(-1129627915442142723l);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getImagesInfo(gov.nasa.pds.Page  page ,)gov.nasa.pds.Restriction  restriction )*
     */
    public gov.nasa.pds.PagedResults getImagesInfo(gov.nasa.pds.Page page,gov.nasa.pds.Restriction restriction) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getImagesInfo");
        System.out.println(page);
        System.out.println(restriction);
        try {
            gov.nasa.pds.PagedResults _return = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnResultsVal1 = null;
            _returnResults.add(_returnResultsVal1);
            _return.getResults().addAll(_returnResults);
            _return.setTotal(3203898387144726580l);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getDataSetRelatedEntitites(long  dataSetId ,)java.lang.String  relatedEntityType )*
     */
    public java.util.List<gov.nasa.pds.EntityInfo> getDataSetRelatedEntitites(long dataSetId,java.lang.String relatedEntityType) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getDataSetRelatedEntitites");
        System.out.println(dataSetId);
        System.out.println(relatedEntityType);
        try {
            java.util.List<gov.nasa.pds.EntityInfo> _return = new java.util.ArrayList<gov.nasa.pds.EntityInfo>();
            gov.nasa.pds.EntityInfo _returnVal1 = new gov.nasa.pds.EntityInfo();
            _returnVal1.setId(-2419006847767883963l);
            _returnVal1.setName("Name-950328884");
            _return.add(_returnVal1);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#searchEntitiesByType(java.lang.String  entityType ,)java.lang.String  searchText ,)gov.nasa.pds.Page  page ,)gov.nasa.pds.Restriction  restriction )*
     */
    public gov.nasa.pds.PagedResults searchEntitiesByType(java.lang.String entityType,java.lang.String searchText,gov.nasa.pds.Page page,gov.nasa.pds.Restriction restriction) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation searchEntitiesByType");
        System.out.println(entityType);
        System.out.println(searchText);
        System.out.println(page);
        System.out.println(restriction);
        try {
            gov.nasa.pds.PagedResults _return = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnResultsVal1 = null;
            _returnResults.add(_returnResultsVal1);
            _return.getResults().addAll(_returnResults);
            _return.setTotal(8414408103534717841l);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getDataSet(long  id )*
     */
    public gov.nasa.pds.DataSet getDataSet(long id) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getDataSet");
        System.out.println(id);
        try {
            gov.nasa.pds.DataSet _return = new gov.nasa.pds.DataSet();
            _return.setId(8633002386168377902l);
            _return.setName("Name-1232380598");
            _return.setDescription("Description1583845228");
            java.util.List<gov.nasa.pds.Instrument> _returnInstruments = new java.util.ArrayList<gov.nasa.pds.Instrument>();
            gov.nasa.pds.Instrument _returnInstrumentsVal1 = new gov.nasa.pds.Instrument();
            _returnInstrumentsVal1.setId(-920352678373463148l);
            _returnInstrumentsVal1.setName("Name-648424624");
            _returnInstrumentsVal1.setDescription("Description-54147852");
            java.util.List<gov.nasa.pds.InstrumentHost> _returnInstrumentsVal1Hosts = new java.util.ArrayList<gov.nasa.pds.InstrumentHost>();
            _returnInstrumentsVal1.getHosts().addAll(_returnInstrumentsVal1Hosts);
            java.util.List<gov.nasa.pds.MetadataObject> _returnInstrumentsVal1OtherChildren = new java.util.ArrayList<gov.nasa.pds.MetadataObject>();
            _returnInstrumentsVal1.getOtherChildren().addAll(_returnInstrumentsVal1OtherChildren);
            java.util.List<gov.nasa.pds.Reference> _returnInstrumentsVal1References = new java.util.ArrayList<gov.nasa.pds.Reference>();
            _returnInstrumentsVal1.getReferences().addAll(_returnInstrumentsVal1References);
            _returnInstrumentsVal1.setTextId("TextId2134424163");
            _returnInstrumentsVal1.setType("Type516537452");
            _returnInstruments.add(_returnInstrumentsVal1);
            _return.getInstruments().addAll(_returnInstruments);
            java.util.List<gov.nasa.pds.Mission> _returnMissions = new java.util.ArrayList<gov.nasa.pds.Mission>();
            gov.nasa.pds.Mission _returnMissionsVal1 = new gov.nasa.pds.Mission();
            _returnMissionsVal1.setId(2991907153886504459l);
            _returnMissionsVal1.setName("Name707292353");
            _returnMissionsVal1.setDescription("Description-1825008316");
            _returnMissionsVal1.setEndDate(javax.xml.datatype.DatatypeFactory.newInstance().newXMLGregorianCalendar("2012-05-14T17:08:08.143+03:00"));
            java.util.List<gov.nasa.pds.MetadataObject> _returnMissionsVal1OtherChildren = new java.util.ArrayList<gov.nasa.pds.MetadataObject>();
            _returnMissionsVal1.getOtherChildren().addAll(_returnMissionsVal1OtherChildren);
            java.util.List<gov.nasa.pds.Reference> _returnMissionsVal1References = new java.util.ArrayList<gov.nasa.pds.Reference>();
            _returnMissionsVal1.getReferences().addAll(_returnMissionsVal1References);
            _returnMissionsVal1.setStartDate(javax.xml.datatype.DatatypeFactory.newInstance().newXMLGregorianCalendar("2012-05-14T17:08:08.143+03:00"));
            _returnMissions.add(_returnMissionsVal1);
            _return.getMissions().addAll(_returnMissions);
            java.util.List<gov.nasa.pds.MetadataObject> _returnOtherChildren = new java.util.ArrayList<gov.nasa.pds.MetadataObject>();
            gov.nasa.pds.MetadataObject _returnOtherChildrenVal1 = new gov.nasa.pds.MetadataObject();
            _returnOtherChildrenVal1.setId(301307681611443788l);
            _returnOtherChildrenVal1.setName("Name20967693");
            java.util.List<gov.nasa.pds.MetadataObject> _returnOtherChildrenVal1Children = new java.util.ArrayList<gov.nasa.pds.MetadataObject>();
            _returnOtherChildrenVal1.getChildren().addAll(_returnOtherChildrenVal1Children);
            java.util.List<gov.nasa.pds.Property> _returnOtherChildrenVal1Properties = new java.util.ArrayList<gov.nasa.pds.Property>();
            _returnOtherChildrenVal1.getProperties().addAll(_returnOtherChildrenVal1Properties);
            _returnOtherChildren.add(_returnOtherChildrenVal1);
            _return.getOtherChildren().addAll(_returnOtherChildren);
            _return.setRating(Float.valueOf(0.2612573f));
            java.util.List<gov.nasa.pds.Reference> _returnReferences = new java.util.ArrayList<gov.nasa.pds.Reference>();
            gov.nasa.pds.Reference _returnReferencesVal1 = new gov.nasa.pds.Reference();
            _returnReferencesVal1.setId(-4824513668685319053l);
            _returnReferencesVal1.setDescription("Description-258778500");
            _returnReferencesVal1.setKeyTextId("KeyTextId-329048405");
            _returnReferences.add(_returnReferencesVal1);
            _return.getReferences().addAll(_returnReferences);
            _return.setStartDate(javax.xml.datatype.DatatypeFactory.newInstance().newXMLGregorianCalendar("2012-05-14T17:08:08.143+03:00"));
            _return.setStopDate(javax.xml.datatype.DatatypeFactory.newInstance().newXMLGregorianCalendar("2012-05-14T17:08:08.143+03:00"));
            java.util.List<gov.nasa.pds.Target> _returnTargets = new java.util.ArrayList<gov.nasa.pds.Target>();
            gov.nasa.pds.Target _returnTargetsVal1 = new gov.nasa.pds.Target();
            _returnTargetsVal1.setId(-7142401187043944965l);
            _returnTargetsVal1.setName("Name-1301421450");
            java.util.List<gov.nasa.pds.Reference> _returnTargetsVal1References = new java.util.ArrayList<gov.nasa.pds.Reference>();
            _returnTargetsVal1.getReferences().addAll(_returnTargetsVal1References);
            java.util.List<gov.nasa.pds.TargetType> _returnTargetsVal1Types = new java.util.ArrayList<gov.nasa.pds.TargetType>();
            _returnTargetsVal1.getTypes().addAll(_returnTargetsVal1Types);
            _returnTargets.add(_returnTargetsVal1);
            _return.getTargets().addAll(_returnTargets);
            _return.setTextId("TextId1321246747");
            java.util.List<gov.nasa.pds.Volume> _returnVolumes = new java.util.ArrayList<gov.nasa.pds.Volume>();
            gov.nasa.pds.Volume _returnVolumesVal1 = new gov.nasa.pds.Volume();
            _returnVolumesVal1.setId(7892071463268333832l);
            _returnVolumesVal1.setName("Name1004080816");
            _returnVolumesVal1.setDescription("Description1033209881");
            java.util.List<gov.nasa.pds.MetadataObject> _returnVolumesVal1OtherChildren = new java.util.ArrayList<gov.nasa.pds.MetadataObject>();
            _returnVolumesVal1.getOtherChildren().addAll(_returnVolumesVal1OtherChildren);
            java.util.List<gov.nasa.pds.Property> _returnVolumesVal1OtherProperties = new java.util.ArrayList<gov.nasa.pds.Property>();
            _returnVolumesVal1.getOtherProperties().addAll(_returnVolumesVal1OtherProperties);
            _returnVolumesVal1.setSeriesName("SeriesName-1672233147");
            _returnVolumesVal1.setSetName("SetName-1905571705");
            _returnVolumesVal1.setSetTextId("SetTextId-265837447");
            _returnVolumesVal1.setTextId("TextId-1823771313");
            _returnVolumes.add(_returnVolumesVal1);
            _return.getVolumes().addAll(_returnVolumes);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getDataSetsInfo(gov.nasa.pds.Page  page ,)gov.nasa.pds.Restriction  restriction )*
     */
    public gov.nasa.pds.PagedResults getDataSetsInfo(gov.nasa.pds.Page page,gov.nasa.pds.Restriction restriction) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getDataSetsInfo");
        System.out.println(page);
        System.out.println(restriction);
        try {
            gov.nasa.pds.PagedResults _return = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnResultsVal1 = null;
            _returnResults.add(_returnResultsVal1);
            _return.getResults().addAll(_returnResults);
            _return.setTotal(1954869499695550099l);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getDataFile(long  id )*
     */
    public gov.nasa.pds.WsDataFile getDataFile(long id) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getDataFile");
        System.out.println(id);
        try {
            gov.nasa.pds.WsDataFile _return = new gov.nasa.pds.WsDataFile();
            _return.setContent("Content2079063378");
            javax.activation.DataHandler _returnDataHandler = null;
            _return.setDataHandler(_returnDataHandler);
            _return.setFilename("Filename208232163");
            _return.setId(-7630342795580305126l);
            _return.setName("Name505843908");
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#getTargetType(long  id )*
     */
    public gov.nasa.pds.TargetType getTargetType(long id) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation getTargetType");
        System.out.println(id);
        try {
            gov.nasa.pds.TargetType _return = new gov.nasa.pds.TargetType();
            _return.setId(2857740345481821816l);
            _return.setName("Name-1121184576");
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

    /* (non-Javadoc)
     * @see gov.nasa.pds.PlanetaryDataSystemSEI#searchEntities(java.lang.String  searchText ,)gov.nasa.pds.Page  page )*
     */
    public gov.nasa.pds.SearchResults searchEntities(java.lang.String searchText,gov.nasa.pds.Page page) throws DataSetProcessingException_Exception    { 
        LOG.info("Executing operation searchEntities");
        System.out.println(searchText);
        System.out.println(page);
        try {
            gov.nasa.pds.SearchResults _return = new gov.nasa.pds.SearchResults();
            gov.nasa.pds.PagedResults _returnDataFiles = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnDataFilesResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnDataFilesResultsVal1 = null;
            _returnDataFilesResults.add(_returnDataFilesResultsVal1);
            _returnDataFiles.getResults().addAll(_returnDataFilesResults);
            _returnDataFiles.setTotal(428624190831024610l);
            _return.setDataFiles(_returnDataFiles);
            gov.nasa.pds.PagedResults _returnDatasets = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnDatasetsResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnDatasetsResultsVal1 = null;
            _returnDatasetsResults.add(_returnDatasetsResultsVal1);
            _returnDatasets.getResults().addAll(_returnDatasetsResults);
            _returnDatasets.setTotal(2411398679739050498l);
            _return.setDatasets(_returnDatasets);
            gov.nasa.pds.PagedResults _returnInstruments = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnInstrumentsResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnInstrumentsResultsVal1 = null;
            _returnInstrumentsResults.add(_returnInstrumentsResultsVal1);
            _returnInstruments.getResults().addAll(_returnInstrumentsResults);
            _returnInstruments.setTotal(4667254637805480595l);
            _return.setInstruments(_returnInstruments);
            gov.nasa.pds.PagedResults _returnMissions = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnMissionsResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnMissionsResultsVal1 = null;
            _returnMissionsResults.add(_returnMissionsResultsVal1);
            _returnMissions.getResults().addAll(_returnMissionsResults);
            _returnMissions.setTotal(-7413342835189536610l);
            _return.setMissions(_returnMissions);
            gov.nasa.pds.PagedResults _returnTargetTypes = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnTargetTypesResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnTargetTypesResultsVal1 = null;
            _returnTargetTypesResults.add(_returnTargetTypesResultsVal1);
            _returnTargetTypes.getResults().addAll(_returnTargetTypesResults);
            _returnTargetTypes.setTotal(1346938715706066420l);
            _return.setTargetTypes(_returnTargetTypes);
            gov.nasa.pds.PagedResults _returnTargets = new gov.nasa.pds.PagedResults();
            java.util.List<java.lang.Object> _returnTargetsResults = new java.util.ArrayList<java.lang.Object>();
            java.lang.Object _returnTargetsResultsVal1 = null;
            _returnTargetsResults.add(_returnTargetsResultsVal1);
            _returnTargets.getResults().addAll(_returnTargetsResults);
            _returnTargets.setTotal(-4830761326768947918l);
            _return.setTargets(_returnTargets);
            return _return;
        } catch (java.lang.Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException(ex);
        }
        //throw new DataSetProcessingException_Exception("DataSetProcessingException...");
    }

}
