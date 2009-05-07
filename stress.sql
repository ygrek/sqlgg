 SELECT	xx.xx_rec_id					promise_unit_ID, 
	xx.xx_volume					aggregate_volume,	
	xx_category					category,	
	xx_difference					difference, 
	xx_initial_liability_amt			initial_liability, 
	xx_out_of_pocket				out_of_pocket, 
	xx_holdings_Secured_Flag			holdings_UNSECURED_FLAG,
	xx.xx_normal_liability_chart			normal_liability_chart,
	xx.xx_facility_liability_chart			FACILITY_liability_chart,	
	yy.yy_rec_i					target_REC_ID,	
	yy.yy_prediction_meter				chart_prediction_meter,
	yy.yy_unique_instance_liability_chart		unique_instance_liability_chart,
	yy.yy_target_liability_chart			target_liability_chart,	
	xx.xx_indicator_id,				
	xx.xx_unused_src_calc				UNUSED_count_SOURCE,	
	yy.yy_target_nm					target_identifier, 
	yy.yy_target_id					target_ID, 
	xx.xx_facility_id				mqxy_promise_unit_ID,
	xx.xx_amt					mqxy_unit_present_count, 
	xx.xx_units					mqxy_unit_present_units, 
	xx.xx_standard_amt				mqxy_standard_promise_count, 
	xx.xx_units					mqxy_standard_unit_units,
	to_CHAR(trunc(xx_during_contract_DT))		during_contract_DATE,
	to_CHAR(trunc(xx_termination_DT))		termination_DATE,
	to_CHAR(trunc(xx.xx_expiration_DT))		expiration_DATE,
	nvl(agreement_aggregate.agreement_aggregate,0)	agreement_aggregate,
	nvl(lc_aggregate.lc_aggregate,0)		LC_aggregate,	
	r.r_xoynx_RTG					xoynxS_RATING,	
	r.r_grrrr_RTG					grrrr_RATING	
 FROM	(SELECT lyy.xx_rec_id,	
	 	SUM(lyy.l_count)			agreement_aggregate	
	FROM	target			yy,	
		promise_agreement	zz, 
		promise_unit		aa,	
		agreement_unit		lo 
	WHERE 	zz.yy_rec_id = yy.yy_rec_id AND
		xx.ca_rec_id = zz.ca_rec_id AND
	 	lo.xx_rec_id = xx.xx_rec_id	
	GROUP BY	yy.yy_rec_id,
			lo.xx_rec_id
	) 			agreement_aggregate, 
	(SELECT lc.xx_rec_id,	
		SUM(lc.lc_amt)		lc_aggregate
	FROM 	target			yy,	
		promise_agreement	zz,	
		promise_unit		aa,	
		lc_unit			lc
	WHERE 	zz.yy_rec_id = yy.yy_rec_id AND
		xx.ca_rec_id = zz.ca_rec_id AND
		lc.xx_rec_id = xx.xx_rec_id
	GROUP BY 	yy.yy_rec_id,	
			lc.xx_rec_id
	) 			lc_aggregate, 
	target			yy,	
	promise_agreement	zz,	
	rating			r, 
	promise_unit		cs 
 WHERE 	zz.yy_rec_id = yy.yy_rec_id AND
	r.yy_rec_id(+) = yy.yy_rec_id AND
	xx.ca_rec_id = zz.ca_rec_id AND
	lc_aggregate.xx_rec_id(+) = xx.xx_rec_id AND
	agreement_aggegate.xx_rec_id(+)	= xx.xx_rec_id AND
	xx.xx_indicator_id = 1 AND
	zz.ca_indicator_id = 1 AND
	(	r.r_indicator_id = 1 OR 
		r.r_indicator_id IS NULL)
 ORDER BY target_identifier
