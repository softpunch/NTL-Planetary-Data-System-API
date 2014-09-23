# PDS Project
-----------------


### LROC DataSets and Products Import Process and Deployment
-------------------------

1. Configure import_and_persistence/config/application.properties (other configuration attributes can be left as it is.)

| Name | Description |
| -----| ------------|
| jdbc.url | MySQL connection string, e.g. jdbc:mysql://localhost:3306/nasa_pds|
| jdbc.username | MySQL username | 
| jdbc.password | MySQL password |
| LROCImportProcessor.requestUrl | The ODE REST API URL used to request JSON string. NOTE THAT THE DEFAULT CONFIGURATION HAS A LIMIT OF 5 IMAGES (limit=5) APPENDED TO THE URL. REMOVE THIS LIMIT TO DOWNLOAD ALL THE IMAGES FOR A GIVEN PRODUCT SET. This is so users do not erroneously download massive amounts of data. |

2. Clear and setup the database in mysql:

       ``` ant reset-db-ddl ```

3. Copy the LROC EDR data set docs locally, this will download the three LROC datasets locally from :

       ``` ./get_dataset_volume_docs.sh ```

4. Process the LROC EDR data set docs into the database, this will process the downloaded LROC data from previous step:

      ```  ant run_cache ``` 
      ```  ant run_processing ``` 

5. Validate the dataset has loaded:

      ``` mysql -u root -p nasa_pds      ``` 
      ``` select count(*) from dataset d inner join dataset_volume dsv on d.id = dsv.dataset_id inner join volume v on dsv.volume_id = v.id      ``` 

      Should return 37 rows

6. Run LROC image URL import tool, we are importing here EDRNAC product type, and we set the camera specs (NAC), the product type will be appended to LROCImportProcessor.requestUrl string 

      ``` chmod +x *.sh      ``` 
      ``` ./run_lroc_import.sh src/app_config/applicationContext_import.xml EDRNAC NAC      ``` 
      
      It is recommended to run this as daemon process if the products count is huge.

7. Check image is in the DB:

      ``` mysql -u root -p nasa_pds      ``` 
      ``` select image_path from map_image      ``` 

8. Check image is linked to the dataset:

      ``` mysql -u root -p nasa_pds     ``` 
      ``` select mi.image_path, mi.product_type, mi.camera_spec from dataset d inner join dataset_map_image dmi on d.id = dmi.dataset_id inner join map_image mi on dmi.map_image_id = mi.id      ``` 


By end of the execution of these steps, the LMMP API will be ready to search LROC images using DataSetService in pds application
