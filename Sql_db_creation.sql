-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

CREATE DATABASE DCDM_GRP1;
USE DCDM_GRP1;

CREATE TABLE Analysis (analysis_id varchar(255) NOT NULL PRIMARY KEY,gene_accession_id varchar(255) NOT NULL,mouse_strain varchar(255),mouse_life_stage varchar(255),parameter_id varchar(255) NOT NULL,pvalue DECIMAL(9,8)NOT NULL);
LOAD DATA LOCAL INFILE 'filepath/analysis_table.csv' INTO TABLE Analysis FIELDS TERMINATED BY ',' ENCLOSED BY '"' LINES TERMINATED BY '\n' IGNORE 1 LINES;

CREATE TABLE Parameter (parameter_id varchar(255) NOT NULL PRIMARY KEY,parameter_name varchar(255) NOT NULL,parameter_description varchar(255),impcParameterOrigId BIGINT ,parameter_group varchar(255) NOT NULL,procedure_mandatory varchar(255),procedure_id varchar(255));
LOAD DATA LOCAL INFILE 'filepath/parameter_table.csv' INTO TABLE Parameter FIELDS TERMINATED BY ',' ENCLOSED BY '"' LINES TERMINATED BY '\n' IGNORE 1 LINES (@vone,@vtwo,@vthree,@vfour,@vfive,@vsix,procedure_id) 
SET
parameter_id = NULLIF(@vone,'NA'),
parameter_name = NULLIF(@vtwo,'NA'),
parameter_description = NULLIF(@vthree,'NA'),
impcParameterOrigId = NULLIF(@vfour,'NA'),
parameter_group = NULLIF(@vfive,'NA'),
procedure_mandatory = NULLIF(@vsix,'NA');

#stores values for eavh columns as variable then sets to NULL if it equals NA

UPDATE Parameter SET procedure_id = NULL WHERE procedure_id ="NA";
ALTER TABLE Parameter MODIFY procedure_id INT;

ALTER TABLE Analysis
ADD FOREIGN KEY (parameter_id) REFERENCES Parameter(parameter_id); 

CREATE TABLE Procedures (procedure_id INT NOT NULL PRIMARY KEY, procedure_name varchar(255), procedure_description varchar(255));
LOAD DATA LOCAL INFILE 'filepath/procedures_table.csv' INTO TABLE Procedures FIELDS TERMINATED BY ',' ENCLOSED BY '"' LINES TERMINATED BY '\n' IGNORE 1 LINES (procedure_id,@two,@three)
SET
procedure_name = NULLIF(@two,' '),
procedure_description = NULLIF(@three,' ');

ALTER TABLE Parameter
ADD FOREIGN KEY (procedure_id) REFERENCES Procedures(procedure_id); 


CREATE TABLE Genes (gene_accession_id varchar(255) NOT NULL PRIMARY KEY, gene_symbol varchar(255) NOT NULL);
LOAD DATA LOCAL INFILE 'filepath/gene_table.csv' INTO TABLE Genes FIELDS TERMINATED BY ',' ENCLOSED BY '"' LINES TERMINATED BY '\n' IGNORE 1 LINES;
ALTER TABLE Analysis
ADD FOREIGN KEY (gene_accession_id) REFERENCES Genes(gene_accession_id);

CREATE TABLE DiseaseGene (id INT NOT NULL PRIMARY KEY, disease_id varchar(255), phenodigm_score FLOAT,gene_accession_id varchar(255));
LOAD DATA LOCAL INFILE 'filepath/diseaseGene_table.csv' INTO TABLE DiseaseGene FIELDS TERMINATED BY ',' ENCLOSED BY '"' LINES TERMINATED BY '\n' IGNORE 1 LINES;

ALTER TABLE DiseaseGene
ADD FOREIGN KEY (gene_accession_id) REFERENCES Genes(gene_accession_id);

CREATE TABLE Disease (disease_id varchar(255) NOT NULL PRIMARY KEY,disease_description varchar (255));
LOAD DATA LOCAL INFILE 'filepath/disease_table.csv' INTO TABLE Disease FIELDS TERMINATED BY ',' ENCLOSED BY '"' LINES TERMINATED BY '\n' IGNORE 1 LINES;

ALTER TABLE DiseaseGene
ADD FOREIGN KEY (disease_id) REFERENCES Disease(disease_id);

CREATE TABLE Group_list (Group_names varchar (255) NOT NULL PRIMARY KEY);
LOAD DATA LOCAL INFILE 'filepath/group_list.csv' INTO TABLE Group_list FIELDS TERMINATED BY ',' ENCLOSED BY '"' LINES TERMINATED BY '\n' IGNORE 1 LINES;

ALTER TABLE Parameter
ADD FOREIGN KEY (parameter_group) REFERENCES Group_list(Group_names);

#example queries
SELECT Analysis.analysis_id,
Analysis.pvalue,
Genes.gene_accession_id,
Genes.gene_symbol,
Parameter.parameter_name,
Parameter.parameter_group,Parameter.parameter_ID,impcParameterOrigId,Procedures.procedure_name
FROM Analysis 
INNER JOIN Genes ON Genes.gene_accession_id=Analysis.gene_accession_id 
INNER JOIN Parameter ON Parameter.parameter_id=Analysis.parameter_id
INNER JOIN Procedures ON Procedures.procedure_id=Parameter.procedure_id
WHERE Genes.gene_symbol='Fahd2a'AND Analysis.pvalue <= 0.06
ORDER BY pvalue DESC;

SELECT Parameter.parameter_id,Parameter.parameter_group,Genes.gene_symbol,Procedures.procedure_name,Analysis.analysis_id,Parameter.impcParameterOrigId
FROM Parameter 
INNER JOIN Procedures ON Procedures.procedure_id=Parameter.procedure_id 
INNER JOIN Analysis ON Analysis.parameter_id=Parameter.parameter_id
INNER JOIN Genes ON Genes.gene_accession_id=Analysis.gene_accession_id
WHERE Parameter.parameter_id='IMPC_CSD_021_001' AND Analysis.pvalue <= 0.05;