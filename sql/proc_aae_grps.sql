-- README
-- Procedure to create dataset for ED arrival mode groups from SUS+ Live db in NCDR
-- proc creates a grouping variable for a user-defined time period - data pulled from 'vw_aae_1045'

USE [NHSE_Sandbox_StrategyUnit];
GO

-- DROP PROCEDURE [dbo].[proc_fetch_aae_grps_1045];
-- GO

SET ANSI_NULLS ON;
GO
SET QUOTED_IDENTIFIER ON;
GO
SET NOCOUNT ON;
GO

CREATE PROCEDURE [dbo].[proc_fetch_aae_grps_1045]
@StartDt DATE,
@EndDt DATE

AS

SELECT
	dataset,
	yyyymm,
	lacd,
	sex,
	age,
	arrmode,
	COUNT(id) n

FROM
	[NHSE_Sandbox_StrategyUnit].[dbo].[vw_aae_1045]

WHERE
	arrdt >= @StartDt
	AND arrdt <= @EndDt

GROUP BY
	dataset,
	yyyymm,
	lacd,
	sex,
	age,
	arrmode;

GO

USE [NHSE_Sandbox_StrategyUnit];
GO

SET NOCOUNT ON

DECLARE @StartDt DATE,
				@EndDt DATE;

SET @StartDt = '2022-01-01';
SET @EndDt = '2022-12-31';

EXEC [dbo].[proc_fetch_aae_grps_1045] @StartDt, @EndDt;

GO
