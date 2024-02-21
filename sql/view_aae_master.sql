-- README
-- Record level ED visits by key variables from SUS+ Live db in NCDR
-- F1 or ctrl + shift + p - MS SQL: Connect - complete connection profile
-- ctrl + shift + e to run the query from VS Code
-- F1 - MS SQL: Cancel query - to cancel a query while it's running

USE [NHSE_Sandbox_StrategyUnit];
GO

-- DROP VIEW [dbo].[vw_aae_1045];
-- GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO
SET NOCOUNT ON;
GO

CREATE VIEW [dbo].[vw_aae_1045]
AS

	SELECT
		'ecds' dataset,
		EC_Ident id,
		CONVERT(DATE, Arrival_Date) arrdt,
		CAST(YEAR(CONVERT(DATE, Arrival_Date)) AS VARCHAR(4)) + RIGHT('0' + CAST(MONTH(Arrival_Date) AS VARCHAR(2)), 2) yyyymm,
		CASE
		WHEN Sex = '1' THEN 'm'
		WHEN Sex = '2' THEN 'f'
		WHEN Sex IN ('0', '9', 'X') THEN 'NA'
		ELSE Sex
	END	sex,
		CASE WHEN Age_at_Arrival > 109 THEN 'NA' ELSE CAST(Age_at_Arrival AS VARCHAR(10)) END age,
		CASE
		WHEN tb2.ArrivalModeKey IN (3, 4, 5, 6, 9) THEN 'amb'
		WHEN tb2.ArrivalModeKey IN (1, 2, 7, 8) THEN 'walkin'
		WHEN tb2.ArrivalModeKey IS NULL THEN 'walkin'
		ELSE CAST(tb2.ArrivalModeKey AS VARCHAR(10))
	END arrmode,
		Der_Postcode_Dist_Unitary_Auth lacd,
		Der_Postcode_LSOA_2011_Code lsoa11cd

	FROM
		[NHSE_SUSPlus_Live].[dbo].[tbl_Data_SUS_EC] tb1
		LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Arrival_Mode] tb2
		ON tb1.EC_Arrival_Mode_SNOMED_CT = tb2.ArrivalModeCode

	WHERE 	
	EC_Department_Type = '01'
		AND Der_Dupe_Flag = 0
		AND Arrival_Date >= '2018-01-01'
		AND Arrival_Date <= '2023-03-31'
		AND LEFT(Der_Postcode_Dist_Unitary_Auth, 1) = 'E';

GO
