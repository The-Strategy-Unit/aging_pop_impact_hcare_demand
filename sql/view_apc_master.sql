-- Record level admissions by key variables from SUS+ Live db in NCDR
-- F1 or ctrl + shift + p - MS SQL: Connect - complete connection profile
-- ctrl + shift + e to run the query from VS Code
-- F1 - MS SQL: Cancel query - to cancel a query while it's running

USE [NHSE_Sandbox_StrategyUnit];
GO

-- DROP VIEW [dbo].[vw_apc_1045];
-- GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO
SET NOCOUNT ON;
GO

CREATE VIEW [dbo].[vw_apc_1045] AS

SELECT
	APCS_Ident id,
	CONVERT(DATE, Admission_Date) admidt,
	CONVERT(DATE, Discharge_Date) disdt,
	CAST(YEAR(CONVERT(date, Discharge_Date)) AS VARCHAR(4)) + RIGHT('0' + CAST(MONTH(Discharge_Date) AS VARCHAR(2)), 2) yyyymm,
  CASE
		WHEN Sex = '1' THEN 'm'
		WHEN Sex = '2' THEN 'f'
		WHEN Sex IN ('0', '9', 'X') THEN 'NA'
		ELSE Sex
	END	sex,
	CASE
		WHEN Age_At_Start_of_Spell_SUS >= 7000 THEN '0' 
		WHEN Age_At_Start_of_Spell_SUS > 109 THEN 'NA'
		ELSE CAST(Age_At_Start_of_Spell_SUS AS VARCHAR(10))
	END age,
	CASE
		WHEN Patient_Classification IN ('3', '4') THEN 'reg'
		WHEN Admission_Method IN ('82', '83') THEN 'birth'
		WHEN (Admission_Method LIKE '3%' OR Der_Admit_Treatment_Function_Code IN ('501', '560')) AND Age_At_Start_of_Spell_SUS > 14 AND Age_At_Start_of_Spell_SUS < 45 THEN 'mat'
		WHEN (Age_At_Start_of_Spell_SUS < 17 OR Age_At_Start_of_Spell_SUS >= 7000) AND Admission_Method LIKE '2%' THEN 'paeds-emer'
		WHEN (Age_At_Start_of_Spell_SUS < 17 OR Age_At_Start_of_Spell_SUS >= 7000) AND Admission_Method IN ('11', '12', '13') AND Patient_Classification = '1' THEN 'paeds-elec'
		WHEN Admission_Method LIKE '2%' THEN 'emer'
		WHEN Admission_Method = '81' THEN 'xfer'
		WHEN Admission_Method IN ('11', '12', '13') AND Patient_Classification = '1' THEN 'ordelec'
		WHEN Admission_Method IN ('11', '12', '13') AND Patient_Classification = '2' THEN 'daycase'
		ELSE 'NA'
	END admigrp,
	Der_Postcode_Dist_Unitary_Auth lacd,
	Der_Postcode_LSOA_2011_Code lsoa11cd,
	Der_Spell_LoS los_sus,
	DATEDIFF(dd, Admission_Date, Discharge_Date) los_dd

FROM
	[NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCS] tb1

WHERE
	Discharge_Date >= '2018-01-01'
	AND Admission_Date <= '2023-03-31'
	AND LEFT(Der_Postcode_Dist_Unitary_Auth, 1) = 'E';

GO
