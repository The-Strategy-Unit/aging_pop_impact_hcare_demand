-- README
-- Record level outpatient attendances by key variables from SUS+ Live db in NCDR
-- F1 or ctrl + shift + p - MS SQL: Connect - complete connection profile
-- ctrl + shift + e to run the query from VS Code
-- F1 - MS SQL: Cancel query - to cancel a query while it's running

USE [NHSE_Sandbox_StrategyUnit];
GO

-- DROP VIEW [dbo].[vw_opc_1045];
-- GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO
SET NOCOUNT ON;
GO

CREATE VIEW [dbo].[vw_opc_1045] AS

SELECT
	OPA_Ident id,
	Appointment_Date appdt,
	CAST(YEAR(CONVERT(date, Appointment_Date)) AS VARCHAR(4)) + RIGHT('0' + CAST(MONTH(Appointment_Date) AS VARCHAR(2)), 2) yyyymm,
  CASE
	WHEN Sex = '1' THEN 'm'
	WHEN Sex = '2' THEN 'f'
	WHEN Sex IN ('0', '9', 'X') THEN 'NA'
	ELSE Sex
	END	sex,
	CASE
		WHEN Age_at_Start_of_Episode_SUS >= 7000 THEN '0' 
		WHEN Age_at_Start_of_Episode_SUS > 109 THEN 'NA'
		ELSE CAST(Age_at_Start_of_Episode_SUS AS VARCHAR(10))
	END age,
	Treatment_Function_Code tretspef,
	Der_Procedure_All procall,
	Der_Number_Procedure procn,
	is_first = CASE First_Attendance
		WHEN '1' THEN 1
		WHEN '2' THEN 0
		WHEN '3' THEN 1
		WHEN '4' THEN 0
		ELSE NULL
  END,
	is_tele = CASE First_Attendance
		WHEN '1' THEN 0
		WHEN '2' THEN 0
		WHEN '3' THEN 1
		WHEN '4' THEN 1
		ELSE NULL
	END,
	is_surg = CASE
		WHEN Treatment_Function_Code IN ('180', '190', '191', '192') THEN 0
		WHEN Treatment_Function_Code LIKE '1%' THEN 1
		ELSE 0
  END,
	has_proc = CASE WHEN EXISTS (
		SELECT 1
		FROM [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_OPA_Proc] opa_proc
    WHERE 
			tb1.OPA_Ident = opa_proc.OPA_Ident
      AND opa_proc.Primary_Procedure_code NOT LIKE '[UXYZ]%'
      AND tb1.First_Attendance NOT IN ('3', '4')
  ) THEN 1
	ELSE 0
	END,
	Der_Postcode_LSOA_2011_Code lsoa11cd,
	Der_Postcode_Dist_Unitary_Auth lacd

FROM
	[NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_OPA] tb1

WHERE
	Appointment_Date >= '2018-01-01'
	AND Appointment_Date <= '2023-03-31'
	AND Der_Attendance_Type = 'Attend'
	AND LEFT(Der_Postcode_Dist_Unitary_Auth, 1) = 'E';

GO
