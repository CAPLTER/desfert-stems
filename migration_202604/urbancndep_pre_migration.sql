--
-- PostgreSQL database dump
--

\restrict Jl2UwL7cdiGlFaEAwFjb4dvDWHZxYbE9IhRXPPJA9rEgZT9cUa6OfJeqOrOFYcF

-- Dumped from database version 18.3 (Ubuntu 18.3-1.pgdg24.04+1)
-- Dumped by pg_dump version 18.3 (Ubuntu 18.3-1.pgdg24.04+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: urbancndep; Type: SCHEMA; Schema: -; Owner: caplter
--

CREATE SCHEMA urbancndep;


ALTER SCHEMA urbancndep OWNER TO caplter;

--
-- Name: trigger_set_timestamp(); Type: FUNCTION; Schema: urbancndep; Owner: caplter
--

CREATE FUNCTION urbancndep.trigger_set_timestamp() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$;


ALTER FUNCTION urbancndep.trigger_set_timestamp() OWNER TO caplter;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: active_admin_comments; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.active_admin_comments (
    id integer NOT NULL,
    namespace character varying(255),
    body text,
    resource_id character varying(255) NOT NULL,
    resource_type character varying(255) NOT NULL,
    author_id integer,
    author_type character varying(255),
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


ALTER TABLE urbancndep.active_admin_comments OWNER TO caplter;

--
-- Name: active_admin_comments_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.active_admin_comments_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.active_admin_comments_id_seq OWNER TO caplter;

--
-- Name: active_admin_comments_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.active_admin_comments_id_seq OWNED BY urbancndep.active_admin_comments.id;


--
-- Name: addtl_site_info_add_site_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.addtl_site_info_add_site_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.addtl_site_info_add_site_id_seq OWNER TO caplter;

--
-- Name: addtl_site_info; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.addtl_site_info (
    add_site_id integer DEFAULT nextval('urbancndep.addtl_site_info_add_site_id_seq'::regclass) NOT NULL,
    site_id integer NOT NULL,
    elevation double precision,
    ozone2003 double precision,
    ozone2004 double precision,
    ozone2005 double precision,
    ozone_avg double precision,
    precipitation2000 double precision,
    precipitation2001 double precision,
    precipitation2002 double precision,
    precipitation2003 double precision,
    precipitation2004 double precision,
    precipitation2005 double precision,
    precipitation_avg double precision,
    jan_temp_max double precision,
    feb_temp_max double precision,
    mar_temp_max double precision,
    apr_temp_max double precision,
    may_temp_max double precision,
    jun_temp_max double precision,
    jul_temp_max double precision,
    aug_temp_max double precision,
    sep_temp_max double precision,
    oct_temp_max double precision,
    nov_temp_max double precision,
    dec_temp_max double precision,
    jan_temp_min double precision,
    feb_temp_min double precision,
    mar_temp_min double precision,
    apr_temp_min double precision,
    may_temp_min double precision,
    jun_temp_min double precision,
    jul_temp_min double precision,
    aug_temp_min double precision,
    sep_temp_min double precision,
    oct_temp_min double precision,
    nov_temp_min double precision,
    dec_temp_min double precision,
    soil_class character varying(100)
);


ALTER TABLE urbancndep.addtl_site_info OWNER TO caplter;

--
-- Name: COLUMN addtl_site_info.add_site_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.add_site_id IS 'new primary key for table';


--
-- Name: COLUMN addtl_site_info.site_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.site_id IS 'new foreign key, references sites';


--
-- Name: COLUMN addtl_site_info.elevation; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.elevation IS 'Elevation above sea level; meter';


--
-- Name: COLUMN addtl_site_info.ozone2003; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.ozone2003 IS '8-hour 4th high measurement in ppm in 2003; dimensionless';


--
-- Name: COLUMN addtl_site_info.ozone2004; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.ozone2004 IS '8-hour 4th high measurement in ppm in 2004; dimensionless';


--
-- Name: COLUMN addtl_site_info.ozone2005; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.ozone2005 IS '8-hour 4th high measurement in ppm in 2005; dimensionless';


--
-- Name: COLUMN addtl_site_info.ozone_avg; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.ozone_avg IS '8-hour 4th high measurement in ppm average 2003 - 2005; dimensionless';


--
-- Name: COLUMN addtl_site_info.precipitation2000; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.precipitation2000 IS 'interpolated precipitation in 2000; millimeter';


--
-- Name: COLUMN addtl_site_info.precipitation2001; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.precipitation2001 IS 'interpolated precipitation in 2001; millimeter';


--
-- Name: COLUMN addtl_site_info.precipitation2002; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.precipitation2002 IS 'interpolated precipitation in 2002; millimeter';


--
-- Name: COLUMN addtl_site_info.precipitation2003; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.precipitation2003 IS 'interpolated precipitation in 2003; millimeter';


--
-- Name: COLUMN addtl_site_info.precipitation2004; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.precipitation2004 IS 'interpolated precipitation in 2004; millimeter';


--
-- Name: COLUMN addtl_site_info.precipitation2005; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.precipitation2005 IS 'interpolated precipitation in 2005; millimeter';


--
-- Name: COLUMN addtl_site_info.precipitation_avg; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.precipitation_avg IS 'interpolated precipitation average 2000 - 2005; millimeter';


--
-- Name: COLUMN addtl_site_info.jan_temp_max; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.jan_temp_max IS 'interpolated maximum temperature in January 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.feb_temp_max; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.feb_temp_max IS 'interpolated maximum temperature in February 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.mar_temp_max; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.mar_temp_max IS 'interpolated maximum temperature in March2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.apr_temp_max; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.apr_temp_max IS 'interpolated maximum temperature in April 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.may_temp_max; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.may_temp_max IS 'interpolated maximum temperature in May 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.jun_temp_max; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.jun_temp_max IS 'interpolated maximum temperature in June 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.jul_temp_max; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.jul_temp_max IS 'interpolated maximum temperature in July 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.aug_temp_max; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.aug_temp_max IS 'interpolated maximum temperature in August 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.sep_temp_max; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.sep_temp_max IS 'interpolated maximum temperature in September 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.oct_temp_max; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.oct_temp_max IS 'interpolated maximum temperature in October 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.nov_temp_max; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.nov_temp_max IS 'interpolated maximum temperature in November 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.dec_temp_max; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.dec_temp_max IS 'interpolated maximum temperature in December 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.jan_temp_min; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.jan_temp_min IS 'interpolated minimum temperature in January 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.feb_temp_min; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.feb_temp_min IS 'interpolated minimum temperature in February 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.mar_temp_min; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.mar_temp_min IS 'interpolated minimum temperature in March 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.apr_temp_min; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.apr_temp_min IS 'interpolated minimum temperature in April 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.may_temp_min; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.may_temp_min IS 'interpolated minimum temperature in May 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.jun_temp_min; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.jun_temp_min IS 'interpolated minimum temperature in June 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.jul_temp_min; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.jul_temp_min IS 'interpolated minimum temperature in July 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.aug_temp_min; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.aug_temp_min IS 'interpolated minimum temperature in August 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.sep_temp_min; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.sep_temp_min IS 'interpolated minimum temperature in September 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.oct_temp_min; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.oct_temp_min IS 'interpolated minimum temperature in October 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.nov_temp_min; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.nov_temp_min IS 'interpolated minimum temperature in November 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.dec_temp_min; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.dec_temp_min IS 'interpolated minimum temperature in December 2000 - 2005 average; celsius ';


--
-- Name: COLUMN addtl_site_info.soil_class; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.addtl_site_info.soil_class IS 'soil classification according to SSURGO database';


--
-- Name: admin_users; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.admin_users (
    id integer NOT NULL,
    email character varying(255) DEFAULT ''::character varying NOT NULL,
    encrypted_password character varying(255) DEFAULT ''::character varying NOT NULL,
    reset_password_token character varying(255),
    reset_password_sent_at timestamp without time zone,
    remember_created_at timestamp without time zone,
    sign_in_count integer DEFAULT 0 NOT NULL,
    current_sign_in_at timestamp without time zone,
    last_sign_in_at timestamp without time zone,
    current_sign_in_ip character varying(255),
    last_sign_in_ip character varying(255),
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    username character varying(255)
);


ALTER TABLE urbancndep.admin_users OWNER TO caplter;

--
-- Name: admin_users_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.admin_users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.admin_users_id_seq OWNER TO caplter;

--
-- Name: admin_users_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.admin_users_id_seq OWNED BY urbancndep.admin_users.id;


--
-- Name: analysis_analysis_test_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.analysis_analysis_test_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.analysis_analysis_test_id_seq OWNER TO caplter;

--
-- Name: analysis; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.analysis (
    analysis_test_id integer DEFAULT nextval('urbancndep.analysis_analysis_test_id_seq'::regclass) NOT NULL,
    analysis character varying(20) DEFAULT ''::character varying NOT NULL,
    storet character varying(6),
    analysis_description character varying(255),
    instrument_code character varying(45),
    instrument character varying(60),
    show_on_web character varying(1)
);


ALTER TABLE urbancndep.analysis OWNER TO caplter;

--
-- Name: COLUMN analysis.analysis_test_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis.analysis_test_id IS 'new primary table';


--
-- Name: analysis_run_data; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.analysis_run_data (
    id integer NOT NULL,
    line_number integer NOT NULL,
    lachat_data_id integer NOT NULL,
    analysis_run_id integer NOT NULL,
    sample_id character varying(23) NOT NULL,
    sample_type character varying(63) NOT NULL,
    replicate_number integer NOT NULL,
    repeat_number integer NOT NULL,
    cup_number character varying(7) NOT NULL,
    manual_dilution_factor integer DEFAULT 1 NOT NULL,
    auto_dilution_factor integer DEFAULT 1 NOT NULL,
    weight integer DEFAULT 1 NOT NULL,
    weight_units character varying(7) DEFAULT 'g'::character varying NOT NULL,
    detection_date date NOT NULL,
    detection_time time without time zone NOT NULL,
    user_name character varying(255),
    run_file_name character varying(255),
    description character varying(255),
    channel_number integer NOT NULL,
    analyte_name character varying(255) NOT NULL,
    peak_concentration double precision NOT NULL,
    concentration_units character varying(15) DEFAULT 'mg N/L'::character varying NOT NULL,
    peak_area double precision NOT NULL,
    peak_height double precision NOT NULL,
    calibration_equation character varying(255),
    retention_time double precision NOT NULL,
    inject_to_peak_start double precision NOT NULL,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


ALTER TABLE urbancndep.analysis_run_data OWNER TO caplter;

--
-- Name: COLUMN analysis_run_data.sample_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.sample_id IS 'output of the lachat from the csv file used to track the compound key as site_id.location_id#replicate_id';


--
-- Name: COLUMN analysis_run_data.sample_type; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.sample_type IS 'output of the lachat, what type of sample is the machine running';


--
-- Name: COLUMN analysis_run_data.replicate_number; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.replicate_number IS 'output of the lachat, number of replicates of a particular sample or analysis during a run';


--
-- Name: COLUMN analysis_run_data.repeat_number; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.repeat_number IS 'output of the lachat, ?';


--
-- Name: COLUMN analysis_run_data.cup_number; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.cup_number IS 'output of the lachat, tracking number for the cups run within each process';


--
-- Name: COLUMN analysis_run_data.manual_dilution_factor; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.manual_dilution_factor IS 'output of the lachat, the dilution value manual injected by operator';


--
-- Name: COLUMN analysis_run_data.auto_dilution_factor; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.auto_dilution_factor IS 'output of the lachat, if auto dilution applied by the machine';


--
-- Name: COLUMN analysis_run_data.weight; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.weight IS 'output of the lachat, weight of the sample in the run ?';


--
-- Name: COLUMN analysis_run_data.weight_units; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.weight_units IS 'output of the lachat, unit of the sample being run ?';


--
-- Name: COLUMN analysis_run_data.detection_date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.detection_date IS 'output of the lachat, date the machine was run';


--
-- Name: COLUMN analysis_run_data.detection_time; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.detection_time IS 'output of the lachat, time the machine was run';


--
-- Name: COLUMN analysis_run_data.user_name; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.user_name IS 'output of the lachat, user name';


--
-- Name: COLUMN analysis_run_data.run_file_name; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.run_file_name IS 'output of lachat, run filename asscoiated with OM_current timestamp';


--
-- Name: COLUMN analysis_run_data.description; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.description IS 'output of the lachat, added description';


--
-- Name: COLUMN analysis_run_data.channel_number; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.channel_number IS 'each run will generate 2 values, channel 1 or channel 2';


--
-- Name: COLUMN analysis_run_data.analyte_name; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.analyte_name IS 'output of the lachat, Name of the Analyte processed - Nitrate or Ammonia';


--
-- Name: COLUMN analysis_run_data.peak_concentration; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.peak_concentration IS 'output of the lachat, final value output by the machine';


--
-- Name: COLUMN analysis_run_data.concentration_units; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.concentration_units IS 'output of the lachat, unit of the final value';


--
-- Name: COLUMN analysis_run_data.peak_area; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.peak_area IS 'output of the lachat, value used to derive the peak_concentration used within the calibration_equation';


--
-- Name: COLUMN analysis_run_data.peak_height; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.peak_height IS 'output of the lachat, ?';


--
-- Name: COLUMN analysis_run_data.calibration_equation; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.calibration_equation IS 'output of the lachat, eqution used to calculate the ammonia/nitrate concentration';


--
-- Name: COLUMN analysis_run_data.retention_time; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.retention_time IS 'output of the lachat, machine diagnostic output reporting';


--
-- Name: COLUMN analysis_run_data.inject_to_peak_start; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_run_data.inject_to_peak_start IS 'output of the lachat, machine diagnostic output reporting';


--
-- Name: analysis_run_data_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.analysis_run_data_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.analysis_run_data_id_seq OWNER TO caplter;

--
-- Name: analysis_run_data_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.analysis_run_data_id_seq OWNED BY urbancndep.analysis_run_data.id;


--
-- Name: analysis_runs; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.analysis_runs (
    run_id integer NOT NULL,
    site_id integer NOT NULL,
    process_date date,
    device_name character varying(63) DEFAULT 'Lachat QC 8000'::character varying,
    operator_name character varying(63) DEFAULT 'CK'::character varying,
    nitrate_calibration_equation character varying(255),
    ammonia_calibration_equation character varying(255),
    run_file_name character varying(255),
    created_on timestamp without time zone NOT NULL,
    updated_on timestamp without time zone,
    lachat_data_id integer,
    date_collected date NOT NULL
);


ALTER TABLE urbancndep.analysis_runs OWNER TO caplter;

--
-- Name: COLUMN analysis_runs.run_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_runs.run_id IS 'primary key used to identify the specific sample processing run.';


--
-- Name: COLUMN analysis_runs.site_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_runs.site_id IS 'foreign key reference to sites';


--
-- Name: COLUMN analysis_runs.process_date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_runs.process_date IS 'Date the samples were processed within the Lachat machine';


--
-- Name: COLUMN analysis_runs.device_name; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_runs.device_name IS 'If the device changes over the course of the study we will have record when and which data was changed';


--
-- Name: COLUMN analysis_runs.operator_name; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_runs.operator_name IS 'user full name or identifier';


--
-- Name: COLUMN analysis_runs.nitrate_calibration_equation; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_runs.nitrate_calibration_equation IS 'channel 1 analysis equation, established at the start of each anyalis run for the NItrate_Nitrite';


--
-- Name: COLUMN analysis_runs.ammonia_calibration_equation; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_runs.ammonia_calibration_equation IS 'channel 2 analysis equation, established at the start of each anyalis run for the Ammonia';


--
-- Name: COLUMN analysis_runs.run_file_name; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_runs.run_file_name IS 'each run writes to a unique file which is exported out with the results';


--
-- Name: COLUMN analysis_runs.created_on; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_runs.created_on IS 'timestamp the data was uploaded';


--
-- Name: COLUMN analysis_runs.updated_on; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.analysis_runs.updated_on IS 'timestamp the data was last updated';


--
-- Name: analysis_runs_run_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.analysis_runs_run_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.analysis_runs_run_id_seq OWNER TO caplter;

--
-- Name: analysis_runs_run_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.analysis_runs_run_id_seq OWNED BY urbancndep.analysis_runs.run_id;


--
-- Name: annuals_biomass; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.annuals_biomass (
    ann_biomass_id integer NOT NULL,
    plot_id integer,
    location_within_plot text,
    replicate integer,
    subquad_orientation text,
    date date,
    year integer,
    mass double precision,
    notes text,
    quadrat integer,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT annuals_biomass_check_plot_location CHECK ((location_within_plot = ANY (ARRAY['P'::text, 'IP'::text]))),
    CONSTRAINT annuals_biomass_check_replicate CHECK ((replicate = ANY (ARRAY[1, 2])))
);


ALTER TABLE urbancndep.annuals_biomass OWNER TO caplter;

--
-- Name: annuals_biomass_ann_biomass_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.annuals_biomass_ann_biomass_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.annuals_biomass_ann_biomass_id_seq OWNER TO caplter;

--
-- Name: annuals_biomass_ann_biomass_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.annuals_biomass_ann_biomass_id_seq OWNED BY urbancndep.annuals_biomass.ann_biomass_id;


--
-- Name: archived_table_stem_length_ambrosia_stem_length_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.archived_table_stem_length_ambrosia_stem_length_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.archived_table_stem_length_ambrosia_stem_length_id_seq OWNER TO caplter;

--
-- Name: archived_table_stem_length_ambrosia; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.archived_table_stem_length_ambrosia (
    stem_length_id integer DEFAULT nextval('urbancndep.archived_table_stem_length_ambrosia_stem_length_id_seq'::regclass) NOT NULL,
    stem_id integer NOT NULL,
    stem_length_mm double precision,
    flag integer
);


ALTER TABLE urbancndep.archived_table_stem_length_ambrosia OWNER TO caplter;

--
-- Name: COLUMN archived_table_stem_length_ambrosia.stem_length_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_length_ambrosia.stem_length_id IS 'Unique identification number';


--
-- Name: COLUMN archived_table_stem_length_ambrosia.stem_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_length_ambrosia.stem_id IS 'foreign key from stems.';


--
-- Name: COLUMN archived_table_stem_length_ambrosia.stem_length_mm; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_length_ambrosia.stem_length_mm IS 'Measured length of the stem;  millimeter';


--
-- Name: archived_table_stem_observations_stem_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.archived_table_stem_observations_stem_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.archived_table_stem_observations_stem_id_seq OWNER TO caplter;

--
-- Name: archived_table_stem_observations; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.archived_table_stem_observations (
    stem_id integer DEFAULT nextval('urbancndep.archived_table_stem_observations_stem_id_seq'::regclass) NOT NULL,
    plot_id integer NOT NULL,
    shrub_id integer NOT NULL,
    sample_date date NOT NULL,
    sample_period integer NOT NULL,
    species_id integer NOT NULL,
    species_code character varying(4) NOT NULL,
    shrub_code character varying(2) DEFAULT ''::character varying NOT NULL,
    stem_direction character varying(7) DEFAULT ''::character varying NOT NULL,
    num_leaves double precision,
    num_inflorescence double precision,
    stem_diameter_mm double precision,
    notes character varying(255),
    adjusted_notes character varying(255)
);


ALTER TABLE urbancndep.archived_table_stem_observations OWNER TO caplter;

--
-- Name: COLUMN archived_table_stem_observations.stem_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.stem_id IS 'primary key';


--
-- Name: COLUMN archived_table_stem_observations.plot_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.plot_id IS 'Plot identification number';


--
-- Name: COLUMN archived_table_stem_observations.shrub_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.shrub_id IS 'foriegn key to the shrubs table';


--
-- Name: COLUMN archived_table_stem_observations.sample_date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.sample_date IS 'Date the sample was taken';


--
-- Name: COLUMN archived_table_stem_observations.sample_period; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.sample_period IS 'Plot identification number';


--
-- Name: COLUMN archived_table_stem_observations.species_code; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.species_code IS 'Species abbreviation';


--
-- Name: COLUMN archived_table_stem_observations.shrub_code; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.shrub_code IS 'Identification consisting of the first letter of the species name and a number 1 through 5 identifying the specific plant';


--
-- Name: COLUMN archived_table_stem_observations.stem_direction; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.stem_direction IS 'Direction the stem is pointing in';


--
-- Name: COLUMN archived_table_stem_observations.num_leaves; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.num_leaves IS 'Number of leaves from the stem marker to the tip';


--
-- Name: COLUMN archived_table_stem_observations.num_inflorescence; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.num_inflorescence IS 'Number of inflorescence observed';


--
-- Name: COLUMN archived_table_stem_observations.stem_diameter_mm; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.stem_diameter_mm IS 'Measured stem diameter;  millimeter';


--
-- Name: COLUMN archived_table_stem_observations.notes; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.notes IS 'Free text';


--
-- Name: COLUMN archived_table_stem_observations.adjusted_notes; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stem_observations.adjusted_notes IS 'Modified notes';


--
-- Name: archived_table_stems_ambrosia_stem_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.archived_table_stems_ambrosia_stem_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.archived_table_stems_ambrosia_stem_id_seq OWNER TO caplter;

--
-- Name: archived_table_stems_ambrosia; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.archived_table_stems_ambrosia (
    stem_id integer DEFAULT nextval('urbancndep.archived_table_stems_ambrosia_stem_id_seq'::regclass) NOT NULL,
    plot_id integer DEFAULT 0 NOT NULL,
    sample_date date NOT NULL,
    sample_period integer DEFAULT 0 NOT NULL,
    species_id integer NOT NULL,
    species_code character varying(4) DEFAULT ''::character varying NOT NULL,
    shrub_id character varying(2) DEFAULT ''::character varying NOT NULL,
    stem_direction character varying(1) DEFAULT ''::character varying NOT NULL,
    num_leaves double precision,
    num_inflorescence double precision,
    stem_diameter_mm double precision,
    notes character varying(255),
    adjusted_notes character varying(255)
);


ALTER TABLE urbancndep.archived_table_stems_ambrosia OWNER TO caplter;

--
-- Name: COLUMN archived_table_stems_ambrosia.stem_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stems_ambrosia.stem_id IS 'primary key';


--
-- Name: COLUMN archived_table_stems_ambrosia.plot_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stems_ambrosia.plot_id IS 'Plot identification number';


--
-- Name: COLUMN archived_table_stems_ambrosia.sample_date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stems_ambrosia.sample_date IS 'Date the sample was taken';


--
-- Name: COLUMN archived_table_stems_ambrosia.sample_period; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stems_ambrosia.sample_period IS 'Periods of monitoring starting at zero and counting up.';


--
-- Name: COLUMN archived_table_stems_ambrosia.species_code; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stems_ambrosia.species_code IS 'Species abbreviation';


--
-- Name: COLUMN archived_table_stems_ambrosia.shrub_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stems_ambrosia.shrub_id IS 'Identification consisting of the first letter of the species name and a number 1 through 5 identifying the specific plant
';


--
-- Name: COLUMN archived_table_stems_ambrosia.stem_direction; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stems_ambrosia.stem_direction IS 'Direction the stem is pointing in';


--
-- Name: COLUMN archived_table_stems_ambrosia.num_leaves; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stems_ambrosia.num_leaves IS 'Number of leaves from the stem marker to the tip';


--
-- Name: COLUMN archived_table_stems_ambrosia.num_inflorescence; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stems_ambrosia.num_inflorescence IS 'Number of inflorescence observed';


--
-- Name: COLUMN archived_table_stems_ambrosia.stem_diameter_mm; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stems_ambrosia.stem_diameter_mm IS 'Measured stem diameter;  millimeter';


--
-- Name: COLUMN archived_table_stems_ambrosia.notes; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stems_ambrosia.notes IS 'Free text';


--
-- Name: COLUMN archived_table_stems_ambrosia.adjusted_notes; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.archived_table_stems_ambrosia.adjusted_notes IS 'Modified notes';


--
-- Name: chn_plant_analysis_chn_analysis_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.chn_plant_analysis_chn_analysis_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.chn_plant_analysis_chn_analysis_id_seq OWNER TO caplter;

--
-- Name: chn_plant_analysis; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.chn_plant_analysis (
    chn_analysis_id integer DEFAULT nextval('urbancndep.chn_plant_analysis_chn_analysis_id_seq'::regclass) NOT NULL,
    sample_id character varying(255),
    analysis_id integer NOT NULL,
    auto_run_num double precision,
    weight_mg double precision,
    analysis_date timestamp without time zone,
    collection_date timestamp without time zone,
    plot_id integer,
    final_value double precision
);


ALTER TABLE urbancndep.chn_plant_analysis OWNER TO caplter;

--
-- Name: TABLE chn_plant_analysis; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON TABLE urbancndep.chn_plant_analysis IS 'CHN analysis of creosote bush plant material';


--
-- Name: COLUMN chn_plant_analysis.chn_analysis_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.chn_plant_analysis.chn_analysis_id IS 'Automatically generated record id number';


--
-- Name: COLUMN chn_plant_analysis.sample_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.chn_plant_analysis.sample_id IS 'Sample id used by analyzer';


--
-- Name: COLUMN chn_plant_analysis.analysis_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.chn_plant_analysis.analysis_id IS 'foreign key reference to plots table ';


--
-- Name: COLUMN chn_plant_analysis.auto_run_num; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.chn_plant_analysis.auto_run_num IS 'Analyzer auto run number, sequential within one run';


--
-- Name: COLUMN chn_plant_analysis.weight_mg; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.chn_plant_analysis.weight_mg IS 'Plant sample dry weight;milligram;0.1; real';


--
-- Name: COLUMN chn_plant_analysis.analysis_date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.chn_plant_analysis.analysis_date IS 'Date of analysis';


--
-- Name: COLUMN chn_plant_analysis.collection_date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.chn_plant_analysis.collection_date IS 'Date plant material was collected';


--
-- Name: COLUMN chn_plant_analysis.plot_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.chn_plant_analysis.plot_id IS 'CNDep plot id';


--
-- Name: COLUMN chn_plant_analysis.final_value; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.chn_plant_analysis.final_value IS 'Element concentration in percent;dimensionless;0.1;real';


--
-- Name: cover_composition; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.cover_composition (
    cover_id integer NOT NULL,
    cover_event_id integer,
    cover_type_id integer,
    cover_amt double precision,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    notes text,
    include boolean
);


ALTER TABLE urbancndep.cover_composition OWNER TO caplter;

--
-- Name: COLUMN cover_composition.include; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.cover_composition.include IS 'indicates whether record should be included in published data';


--
-- Name: cover_composition_cover_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.cover_composition_cover_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.cover_composition_cover_id_seq OWNER TO caplter;

--
-- Name: cover_composition_cover_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.cover_composition_cover_id_seq OWNED BY urbancndep.cover_composition.cover_id;


--
-- Name: cover_events; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.cover_events (
    cover_event_id integer NOT NULL,
    sample_date date,
    year integer,
    plot integer,
    patch_type text,
    subplot integer,
    collector text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE urbancndep.cover_events OWNER TO caplter;

--
-- Name: cover_events_cover_event_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.cover_events_cover_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.cover_events_cover_event_id_seq OWNER TO caplter;

--
-- Name: cover_events_cover_event_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.cover_events_cover_event_id_seq OWNED BY urbancndep.cover_events.cover_event_id;


--
-- Name: cover_types; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.cover_types (
    cover_type_id integer NOT NULL,
    cover_category text,
    cover_type text,
    year_added integer,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    comment text,
    include boolean
);


ALTER TABLE urbancndep.cover_types OWNER TO caplter;

--
-- Name: COLUMN cover_types.year_added; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.cover_types.year_added IS 'year when cover type was first used in observations';


--
-- Name: COLUMN cover_types.comment; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.cover_types.comment IS 'storage or processing comment';


--
-- Name: COLUMN cover_types.include; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.cover_types.include IS 'indicates whether value should be included in published data';


--
-- Name: cover_types_cover_type_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.cover_types_cover_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.cover_types_cover_type_id_seq OWNER TO caplter;

--
-- Name: cover_types_cover_type_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.cover_types_cover_type_id_seq OWNED BY urbancndep.cover_types.cover_type_id;


--
-- Name: fertilizer_applications_application_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.fertilizer_applications_application_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.fertilizer_applications_application_id_seq OWNER TO caplter;

--
-- Name: fertilizer_applications; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.fertilizer_applications (
    id integer DEFAULT nextval('urbancndep.fertilizer_applications_application_id_seq'::regclass) NOT NULL,
    site_id integer NOT NULL,
    date date NOT NULL,
    "N" double precision DEFAULT 60 NOT NULL,
    "P" double precision DEFAULT 12 NOT NULL,
    "N_and_P" character varying(255) DEFAULT '60 and 12'::character varying NOT NULL,
    created_at timestamp without time zone DEFAULT now(),
    updated_at timestamp without time zone DEFAULT now()
);


ALTER TABLE urbancndep.fertilizer_applications OWNER TO caplter;

--
-- Name: COLUMN fertilizer_applications.site_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.fertilizer_applications.site_id IS 'new foreign key to the table.';


--
-- Name: COLUMN fertilizer_applications.date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.fertilizer_applications.date IS 'date of fertilizer application to plot, can change this to date only if preferable';


--
-- Name: COLUMN fertilizer_applications."N"; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.fertilizer_applications."N" IS 'annual application rate (kg N ha-1 y-1); 3.429 kg NH4NO3 per plot per visit';


--
-- Name: COLUMN fertilizer_applications."P"; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.fertilizer_applications."P" IS 'annual P application rate (kg P ha-1 y-1)';


--
-- Name: COLUMN fertilizer_applications."N_and_P"; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.fertilizer_applications."N_and_P" IS 'annual N (kg N ha-1 y-1) and P (kg P ha-1 y-1) application rates';


--
-- Name: lachat_data; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.lachat_data (
    id integer NOT NULL,
    name character varying(255),
    path character varying(255),
    size integer,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    file_name character varying(255),
    uploaded_by character varying(255) NOT NULL,
    file_digest character varying(255) NOT NULL
);


ALTER TABLE urbancndep.lachat_data OWNER TO caplter;

--
-- Name: COLUMN lachat_data.file_digest; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.lachat_data.file_digest IS 'Hash of the uploaded CSV file';


--
-- Name: lachat_data_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.lachat_data_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.lachat_data_id_seq OWNER TO caplter;

--
-- Name: lachat_data_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.lachat_data_id_seq OWNED BY urbancndep.lachat_data.id;


--
-- Name: lachat_output_machine_analysis_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.lachat_output_machine_analysis_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.lachat_output_machine_analysis_id_seq OWNER TO caplter;

--
-- Name: sites_site_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.sites_site_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.sites_site_id_seq OWNER TO caplter;

--
-- Name: sites; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.sites (
    id integer DEFAULT nextval('urbancndep.sites_site_id_seq'::regclass) NOT NULL,
    code character varying(3) DEFAULT ''::character varying NOT NULL,
    name character varying(255) DEFAULT ''::character varying NOT NULL,
    plots_description character varying(255),
    region character varying(255) DEFAULT ''::character varying NOT NULL,
    utm_e double precision,
    utm_n double precision,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


ALTER TABLE urbancndep.sites OWNER TO caplter;

--
-- Name: COLUMN sites.id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.sites.id IS 'primary key with uniqueness ';


--
-- Name: COLUMN sites.code; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.sites.code IS 'Unique site abbreviations';


--
-- Name: COLUMN sites.name; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.sites.name IS 'Unique site abbreviations';


--
-- Name: COLUMN sites.plots_description; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.sites.plots_description IS 'Names of the plots within each site';


--
-- Name: COLUMN sites.region; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.sites.region IS 'Unique site abbreviations';


--
-- Name: COLUMN sites.utm_e; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.sites.utm_e IS 'UTM coordinates - Eastings';


--
-- Name: COLUMN sites.utm_n; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.sites.utm_n IS 'UTM coordinates - Northings';


--
-- Name: lens_fertilizer_applications; Type: VIEW; Schema: urbancndep; Owner: caplter
--

CREATE VIEW urbancndep.lens_fertilizer_applications AS
 SELECT s.code AS site_code,
    s.name AS site_name,
    s.plots_description,
    s.region,
    s.utm_e,
    s.utm_n,
    fa.id AS fertilizer_application_id,
    fa.date AS application_date,
    fa."N" AS nitrogen_amount,
    fa."P" AS phosphorus_amount,
    fa."N_and_P" AS nitrogen_phosphorus_amount
   FROM (urbancndep.sites s
     JOIN urbancndep.fertilizer_applications fa ON ((s.id = fa.site_id)))
  ORDER BY fa.date;


ALTER VIEW urbancndep.lens_fertilizer_applications OWNER TO caplter;

--
-- Name: VIEW lens_fertilizer_applications; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON VIEW urbancndep.lens_fertilizer_applications IS '{"title": "Research site fertilizer application quantities","abstract": "CNdep study plots are fertilized twice per year in December-January and again in May-Jun prior to the rainy seasons. Fertilization began in year 1 and has continued over the lifetime of this project to ensure a consistent press despite interannual climatic variability. The nitrogen fertilization rate in each of the N and N+P study plots is 60 kg N ha^-1 y^-1, calculated as 3429 g NH4NO3 / 400 m2 * (28 mol N / 80 mol NH4NO3) * (1 kg / 1000 g) * (10000 m2 / ha) = 30 kg N ha^-1 application-1 * 2 applications / y = 60 kg N ha^-1 y^-1. The phosphorus fertilization rate in each of the P and N+P plots is 12 kg P ha^-1 y^-1, calculated as 1224 g triple-super-phospate application^-1 * 2 applications / y.","temporal": [{"type": "single","begin": "2008-01-01"}]}';


--
-- Name: plots_plot_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.plots_plot_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.plots_plot_id_seq OWNER TO caplter;

--
-- Name: plots; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.plots (
    id integer DEFAULT nextval('urbancndep.plots_plot_id_seq'::regclass) NOT NULL,
    site_id integer NOT NULL,
    treatment_id integer NOT NULL,
    method_code character varying(255) NOT NULL,
    description text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


ALTER TABLE urbancndep.plots OWNER TO caplter;

--
-- Name: COLUMN plots.id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.plots.id IS 'primary key for the table, plot unique identification';


--
-- Name: COLUMN plots.site_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.plots.site_id IS 'foreign key references sites';


--
-- Name: COLUMN plots.treatment_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.plots.treatment_id IS 'foreign key references treatments';


--
-- Name: COLUMN plots.method_code; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.plots.method_code IS 'coded value for the method, description detials method, (old)chn_sample.sample_id';


--
-- Name: COLUMN plots.description; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.plots.description IS 'details and comments, (old)chn_sample.description';


--
-- Name: shrub_species_species_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.shrub_species_species_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.shrub_species_species_id_seq OWNER TO caplter;

--
-- Name: shrub_species; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.shrub_species (
    id integer DEFAULT nextval('urbancndep.shrub_species_species_id_seq'::regclass) NOT NULL,
    code character varying(4) DEFAULT ''::character varying NOT NULL,
    scientific_name character varying(255)
);


ALTER TABLE urbancndep.shrub_species OWNER TO caplter;

--
-- Name: COLUMN shrub_species.id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_species.id IS 'primary key for the table';


--
-- Name: COLUMN shrub_species.code; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_species.code IS 'Species abbreviation';


--
-- Name: COLUMN shrub_species.scientific_name; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_species.scientific_name IS 'Scientific name of the species';


--
-- Name: shrubs_shrub_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.shrubs_shrub_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.shrubs_shrub_id_seq OWNER TO caplter;

--
-- Name: shrubs; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.shrubs (
    id integer DEFAULT nextval('urbancndep.shrubs_shrub_id_seq'::regclass) NOT NULL,
    plot_id integer NOT NULL,
    shrub_species_id integer NOT NULL,
    code character varying(3) NOT NULL,
    note text,
    created_at timestamp without time zone DEFAULT now(),
    updated_at timestamp without time zone DEFAULT now()
);


ALTER TABLE urbancndep.shrubs OWNER TO caplter;

--
-- Name: COLUMN shrubs.id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrubs.id IS 'unique identification';


--
-- Name: COLUMN shrubs.plot_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrubs.plot_id IS 'foreign key to plots';


--
-- Name: COLUMN shrubs.shrub_species_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrubs.shrub_species_id IS 'foreign key to species_lookup';


--
-- Name: COLUMN shrubs.code; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrubs.code IS 'coded values to track plants within each plot';


--
-- Name: COLUMN shrubs.note; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrubs.note IS 'comments specific to the shrub, death, etc.';


--
-- Name: stem_lengths_stem_length_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.stem_lengths_stem_length_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.stem_lengths_stem_length_id_seq OWNER TO caplter;

--
-- Name: stem_lengths; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.stem_lengths (
    id integer DEFAULT nextval('urbancndep.stem_lengths_stem_length_id_seq'::regclass) NOT NULL,
    stem_id integer NOT NULL,
    length_in_mm double precision,
    flag integer,
    created_at timestamp without time zone DEFAULT now(),
    updated_at timestamp without time zone DEFAULT now(),
    post_measurement boolean DEFAULT false NOT NULL
);


ALTER TABLE urbancndep.stem_lengths OWNER TO caplter;

--
-- Name: COLUMN stem_lengths.id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_lengths.id IS 'Unique identification number';


--
-- Name: COLUMN stem_lengths.stem_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_lengths.stem_id IS 'foreign key from stems.';


--
-- Name: COLUMN stem_lengths.length_in_mm; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_lengths.length_in_mm IS 'Measured length of the stem;  millimeter';


--
-- Name: COLUMN stem_lengths.post_measurement; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_lengths.post_measurement IS 'pre length measurements will be 20 per plot per for all 60 sites each cycle, post length will be the growth measured per bush.';


--
-- Name: stems_stem_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.stems_stem_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.stems_stem_id_seq OWNER TO caplter;

--
-- Name: stems; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.stems (
    id integer DEFAULT nextval('urbancndep.stems_stem_id_seq'::regclass) NOT NULL,
    sample_period integer,
    shrub_id integer NOT NULL,
    direction character varying(5) NOT NULL,
    pre_date date,
    post_date date,
    old_pre_stem_id integer,
    old_post_stem_id integer,
    post_note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT new_stems_stem_direction_check CHECK (((direction)::text = ANY (ARRAY[('East'::character varying)::text, ('West'::character varying)::text, ('North'::character varying)::text, ('South'::character varying)::text])))
);


ALTER TABLE urbancndep.stems OWNER TO caplter;

--
-- Name: COLUMN stems.id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stems.id IS 'primary key for each stem, every observation period will generate new stem_ids used for both';


--
-- Name: COLUMN stems.sample_period; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stems.sample_period IS 'what sample collection period is it.';


--
-- Name: COLUMN stems.shrub_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stems.shrub_id IS 'foriegn key reference to shrubs, required at least 4 stems per shrub_id';


--
-- Name: COLUMN stems.direction; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stems.direction IS 'The direction the stem is pointed in the pre phase';


--
-- Name: COLUMN stems.pre_date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stems.pre_date IS 'date the new marking tape is placed on the branch with 1 observed measurement made';


--
-- Name: COLUMN stems.post_date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stems.post_date IS 'the date the growth recordings are collected';


--
-- Name: COLUMN stems.old_pre_stem_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stems.old_pre_stem_id IS 'delete after the data juggle';


--
-- Name: COLUMN stems.old_post_stem_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stems.old_post_stem_id IS 'delete after the data juggle';


--
-- Name: COLUMN stems.post_note; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stems.post_note IS 'field note observations specific to the stem in question';


--
-- Name: treatments_treatment_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.treatments_treatment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.treatments_treatment_id_seq OWNER TO caplter;

--
-- Name: treatments; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.treatments (
    id integer DEFAULT nextval('urbancndep.treatments_treatment_id_seq'::regclass) NOT NULL,
    code character varying(255) DEFAULT ''::character varying NOT NULL,
    description text,
    created_at timestamp without time zone,
    updated_at timestamp without time zone
);


ALTER TABLE urbancndep.treatments OWNER TO caplter;

--
-- Name: COLUMN treatments.id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.treatments.id IS 'primary key to the table.';


--
-- Name: COLUMN treatments.code; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.treatments.code IS 'Type of fertilizer used for each plot: Ammonium nitrate (N), Triple super phosphate (P), or both (NP).';


--
-- Name: COLUMN treatments.description; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.treatments.description IS 'Description of the treatment';


--
-- Name: lens_plant_growth; Type: VIEW; Schema: urbancndep; Owner: caplter
--

CREATE VIEW urbancndep.lens_plant_growth AS
 SELECT s.code AS site_code,
    s.name AS site_name,
    s.plots_description,
    s.region,
    s.utm_e,
    s.utm_n,
    p.id AS plot_id,
    p.method_code,
    t.code AS treatment_code,
    t.description AS treatment_description,
    sp.code AS shrub_species_code,
    sp.scientific_name,
    sh.code AS shrub_code,
    sh.note AS shrub_note,
    st.id AS stem_id,
    st.direction,
    st.pre_date,
    st.post_date,
    st.post_note,
    sl.post_measurement,
    sl.length_in_mm
   FROM ((((((urbancndep.sites s
     JOIN urbancndep.plots p ON ((s.id = p.site_id)))
     JOIN urbancndep.treatments t ON ((p.treatment_id = t.id)))
     JOIN urbancndep.shrubs sh ON ((p.id = sh.plot_id)))
     JOIN urbancndep.shrub_species sp ON ((sh.shrub_species_id = sp.id)))
     JOIN urbancndep.stems st ON ((sh.id = st.shrub_id)))
     JOIN urbancndep.stem_lengths sl ON ((st.id = sl.stem_id)))
  ORDER BY st.pre_date;


ALTER VIEW urbancndep.lens_plant_growth OWNER TO caplter;

--
-- Name: VIEW lens_plant_growth; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON VIEW urbancndep.lens_plant_growth IS '{"title":"Plant growth, biovolume, and diversity, Larrea tridentata, 2005 to current","abstract":"The CAP LTER monitors the growth (stem elonation) of Larrea tridentata, a ubiquitious desert shrub common among all UrbanCNdep study sites, as an ecosystem response variable to landscape position and nutrient amendments. Stem elongation of Ambrosia deltoidea and Ambrosia dumosa was measured concurrently for a limited duration (2005 - 2008). Additional measures of growth for both Larrea tridentata and the two Ambrosiaspecies, also for a limited duration (2005 - 2008), included biannual measures of leaf and inflorecence number, and stem diamater. Additional botanical data for this project includes a survey of and biovolume data for all perrenial plants in select plots at a limited number of locations.","temporal":[{"type":"single","begin":"2005-01-01"}],"methods":[{"type":"para","value":"Growth of Larrea tridentata is measured as apical stem elongation. Biannual measures include assessing a single stem in each cardinal direction on five study plants in each of four study plots at the fifteen UrbanCNdep study locations. For a detailed project description and methods, refer to Hall et al. 2011 and Sponseller et al. 2012:"},{"type":"citation","value":"Hall, S. J., R. A. Sponseller, N. B. Grimm, D. P. Huber, J. P. Kaye, C. F. Clark and J. P. Collins. 2011. Ecosystem response to nutrient enrichment across an urban airshed in the Sonoran Desert. Ecological Applications 21(3):640-660."},{"type":"citation","value":"Sponseller, R. A., S. J. Hall, D. P. Huber, N. B. Grimm, J. P. Kaye, C. M. Clark and S. L. Collins. 2012. Variation in monsoon precipitation drives spatial and temporal patterns of Larrea tridentata growth in the Sonoran Desert. Functional Ecology 26(3):750-758. DOI: DOI: 10.1111/j.1365-2435.2012.01979.x."}]}';


--
-- Name: lens_plant_growth_ambrosia; Type: VIEW; Schema: urbancndep; Owner: caplter
--

CREATE VIEW urbancndep.lens_plant_growth_ambrosia AS
 SELECT s.code AS site_code,
    s.name AS site_name,
    s.plots_description,
    s.region,
    s.utm_e,
    s.utm_n,
    p.id AS plot_id,
    p.method_code,
    t.code AS treatment_code,
    t.description AS treatment_description,
    ast.stem_id,
    ast.sample_date,
    ast.sample_period,
    ss.code AS species_code,
    ss.scientific_name,
    ast.shrub_id AS shrub_code,
    ast.stem_direction,
    ast.num_leaves,
    ast.num_inflorescence,
    ast.stem_diameter_mm,
    ast.notes,
    ast.adjusted_notes,
    asl.stem_length_id,
    asl.stem_length_mm,
    asl.flag
   FROM (((((urbancndep.sites s
     JOIN urbancndep.plots p ON ((s.id = p.site_id)))
     JOIN urbancndep.treatments t ON ((p.treatment_id = t.id)))
     JOIN urbancndep.archived_table_stems_ambrosia ast ON ((p.id = ast.plot_id)))
     JOIN urbancndep.shrub_species ss ON ((ast.species_id = ss.id)))
     JOIN urbancndep.archived_table_stem_length_ambrosia asl ON ((ast.stem_id = asl.stem_id)))
  ORDER BY ast.sample_date;


ALTER VIEW urbancndep.lens_plant_growth_ambrosia OWNER TO caplter;

--
-- Name: VIEW lens_plant_growth_ambrosia; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON VIEW urbancndep.lens_plant_growth_ambrosia IS '{"title":"Plant growth, biovolume, and diversity, Ambrosia species, 2005 to 2008","abstract":"The CAP LTER monitors the growth (stem elonation) of Larrea tridentata, a ubiquitious desert shrub common among all UrbanCNdep study sites, as an ecosystem response variable to landscape position and nutrient amendments. Stem elongation of Ambrosia deltoidea and Ambrosia dumosa was measured concurrently for a limited duration (2005 - 2008). Additional measures of growth for both Larrea tridentata and the two Ambrosiaspecies, also for a limited duration (2005 - 2008), included biannual measures of leaf and inflorecence number, and stem diamater. Additional botanical data for this project includes a survey of and biovolume data for all perrenial plants in select plots at a limited number of locations.","temporal":[{"type":"range","begin":"2005-01-01","end":"2008-12-31"}],"methods":[{"type":"para","value":"Growth of Larrea tridentata is measured as apical stem elongation. Biannual measures include assessing a single stem in each cardinal direction on five study plants in each of four study plots at the fifteen UrbanCNdep study locations. For a detailed project description and methods, refer to Hall et al. 2011 and Sponseller et al. 2012:"},{"type":"citation","value":"Hall, S. J., R. A. Sponseller, N. B. Grimm, D. P. Huber, J. P. Kaye, C. F. Clark and J. P. Collins. 2011. Ecosystem response to nutrient enrichment across an urban airshed in the Sonoran Desert. Ecological Applications 21(3):640-660."},{"type":"citation","value":"Sponseller, R. A., S. J. Hall, D. P. Huber, N. B. Grimm, J. P. Kaye, C. M. Clark and S. L. Collins. 2012. Variation in monsoon precipitation drives spatial and temporal patterns of Larrea tridentata growth in the Sonoran Desert. Functional Ecology 26(3):750-758. DOI: DOI: 10.1111/j.1365-2435.2012.01979.x."}]}';


--
-- Name: prs_analysis_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.prs_analysis_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.prs_analysis_id_seq OWNER TO caplter;

--
-- Name: prs_analysis; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.prs_analysis (
    id integer DEFAULT nextval('urbancndep.prs_analysis_id_seq'::regclass) NOT NULL,
    wal_id integer,
    plot_id integer,
    start_date date,
    end_date date,
    analyte character varying(255),
    final_value double precision,
    flag character varying(100),
    location_within_plot character varying(50),
    num_cation_probes bigint,
    num_anion_probes bigint,
    notes text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE urbancndep.prs_analysis OWNER TO caplter;

--
-- Name: COLUMN prs_analysis.wal_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_analysis.wal_id IS 'probe ID';


--
-- Name: COLUMN prs_analysis.plot_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_analysis.plot_id IS 'plot ID';


--
-- Name: COLUMN prs_analysis.start_date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_analysis.start_date IS 'date the probe was placed in the field';


--
-- Name: COLUMN prs_analysis.end_date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_analysis.end_date IS 'date the probe was removed from the field';


--
-- Name: COLUMN prs_analysis.analyte; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_analysis.analyte IS 'chemical analyzed';


--
-- Name: COLUMN prs_analysis.final_value; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_analysis.final_value IS 'concentration of chemical in ppm;dimensionless';


--
-- Name: COLUMN prs_analysis.flag; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_analysis.flag IS 'flag of problems';


--
-- Name: COLUMN prs_analysis.location_within_plot; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_analysis.location_within_plot IS 'location within the plot, between plants or under plant';


--
-- Name: COLUMN prs_analysis.num_cation_probes; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_analysis.num_cation_probes IS 'number of probes combined for result';


--
-- Name: COLUMN prs_analysis.num_anion_probes; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_analysis.num_anion_probes IS 'number of probes combined for result';


--
-- Name: lens_prs_probe_analysis; Type: VIEW; Schema: urbancndep; Owner: caplter
--

CREATE VIEW urbancndep.lens_prs_probe_analysis AS
 SELECT s.code AS site_code,
    s.name AS site_name,
    s.plots_description,
    s.region,
    s.utm_e,
    s.utm_n,
    p.id AS plot_id,
    p.method_code,
    t.code AS treatment_code,
    t.description AS treatment_description,
    pa.id AS prs_analysis_id,
    pa.wal_id,
    pa.start_date,
    pa.end_date,
    pa.analyte,
    pa.final_value,
    pa.flag,
    pa.location_within_plot,
    pa.num_cation_probes,
    pa.num_anion_probes
   FROM (((urbancndep.sites s
     JOIN urbancndep.plots p ON ((s.id = p.site_id)))
     JOIN urbancndep.treatments t ON ((p.treatment_id = t.id)))
     JOIN urbancndep.prs_analysis pa ON ((p.id = pa.plot_id)))
  ORDER BY pa.start_date;


ALTER VIEW urbancndep.lens_prs_probe_analysis OWNER TO caplter;

--
-- Name: VIEW lens_prs_probe_analysis; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON VIEW urbancndep.lens_prs_probe_analysis IS '{"title":"Soil Chemistry, Plant Root Simulator","abstract":"Soil properties are assessed in each of two control (C1 and C2) study plots in each of the 15 CNdep study locations semiannually using Plant Root Simulator (PRS) probes provided by WesternAg Innovations (http://www.westernag.ca/innovations). Probes are typically used to assess soil nitrogen (NO3- and NH4+), however a broader suite of cations and anions are assessed at irregular, periodic intervals.","temporal":[{"type":"single","begin":"2008-01-01"}],"methods":[{"type":"para", "value":"PRS probes are deployed semiannually, December and June, to coincide with the rainy seasons, and left in place for ~10 weeks."},{"type":"url","value":"http://www.westernag.ca/innovations"}]}';


--
-- Name: resin_locations_location_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.resin_locations_location_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.resin_locations_location_id_seq OWNER TO caplter;

--
-- Name: resin_locations; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.resin_locations (
    location_id integer DEFAULT nextval('urbancndep.resin_locations_location_id_seq'::regclass) NOT NULL,
    location_code character varying(10) DEFAULT ''::character varying NOT NULL,
    description character varying(100) DEFAULT ''::character varying NOT NULL
);


ALTER TABLE urbancndep.resin_locations OWNER TO caplter;

--
-- Name: TABLE resin_locations; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON TABLE urbancndep.resin_locations IS 'InnoDB free: 132096 kB';


--
-- Name: COLUMN resin_locations.location_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.resin_locations.location_id IS 'primary key for the table';


--
-- Name: COLUMN resin_locations.location_code; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.resin_locations.location_code IS 'location code, older id';


--
-- Name: soil_ph_samples_sample_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.soil_ph_samples_sample_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.soil_ph_samples_sample_id_seq OWNER TO caplter;

--
-- Name: soil_ph_samples; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.soil_ph_samples (
    id integer DEFAULT nextval('urbancndep.soil_ph_samples_sample_id_seq'::regclass) NOT NULL,
    date date NOT NULL,
    plot_id integer NOT NULL,
    extract_type character varying NOT NULL,
    extract_concentration real NOT NULL,
    extract_volume integer NOT NULL,
    location_id integer,
    soil_ph real NOT NULL,
    temperature real NOT NULL,
    slope real,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    processing_notes text
);


ALTER TABLE urbancndep.soil_ph_samples OWNER TO caplter;

--
-- Name: COLUMN soil_ph_samples.date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.soil_ph_samples.date IS 'date of sample';


--
-- Name: COLUMN soil_ph_samples.plot_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.soil_ph_samples.plot_id IS 'Plot number as per the sites table';


--
-- Name: COLUMN soil_ph_samples.extract_type; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.soil_ph_samples.extract_type IS 'Chemical formula of extract type (abbreviation)';


--
-- Name: COLUMN soil_ph_samples.extract_concentration; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.soil_ph_samples.extract_concentration IS 'Concentration of extract type (expressed  in moles)';


--
-- Name: COLUMN soil_ph_samples.extract_volume; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.soil_ph_samples.extract_volume IS 'extract volume in mL';


--
-- Name: COLUMN soil_ph_samples.location_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.soil_ph_samples.location_id IS 'species as per species lookup table, Intended to show where the sample was collected: near a plant or from an inter-plant space.';


--
-- Name: COLUMN soil_ph_samples.soil_ph; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.soil_ph_samples.soil_ph IS 'Measured soi pH value to 2 decimal places';


--
-- Name: COLUMN soil_ph_samples.temperature; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.soil_ph_samples.temperature IS 'soil temperature to one decimal place';


--
-- Name: COLUMN soil_ph_samples.slope; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.soil_ph_samples.slope IS 'Slope produced by instrument during analysis';


--
-- Name: COLUMN soil_ph_samples.processing_notes; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.soil_ph_samples.processing_notes IS 'As issues come up with the sample processing the lab tech makes notes on those issues and those are documented here from anomalies within the data.';


--
-- Name: lens_soil_ph; Type: VIEW; Schema: urbancndep; Owner: caplter
--

CREATE VIEW urbancndep.lens_soil_ph AS
 SELECT s.code AS site_code,
    s.name AS site_name,
    s.plots_description,
    s.region,
    s.utm_e,
    s.utm_n,
    p.id AS plot_id,
    p.method_code,
    t.code AS treatment_code,
    t.description AS treatment_description,
    sps.id AS soil_ph_sample_id,
    sps.date AS soil_ph_sample_date,
    sps.extract_type,
    sps.extract_concentration,
    sps.extract_volume,
    rl.location_code,
    rl.description AS location_description,
    sps.soil_ph,
    sps.temperature,
    sps.slope,
    sps.processing_notes
   FROM ((((urbancndep.sites s
     JOIN urbancndep.plots p ON ((s.id = p.site_id)))
     JOIN urbancndep.treatments t ON ((p.treatment_id = t.id)))
     JOIN urbancndep.soil_ph_samples sps ON ((p.id = sps.plot_id)))
     JOIN urbancndep.resin_locations rl ON ((sps.location_id = rl.location_id)))
  ORDER BY sps.date;


ALTER VIEW urbancndep.lens_soil_ph OWNER TO caplter;

--
-- Name: VIEW lens_soil_ph; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON VIEW urbancndep.lens_soil_ph IS '{"title":"Soil pH","abstract":"soil pH of CNdep study plots was measured during the study years 2008-2011; this analysis has been discontinued to minimize destructive sampling in the relatively small study plots.","temporal":[{"type":"range","begin":"2008-01-01","end":"2011-12-31"}],"methods":[{"type":"para","value":"Soil pH was measured according to methods detailed in Hall et al. 2011:"},{"type":"citation","value":"Hall, S. J., R. A. Sponseller, N. B. Grimm, D. P. Huber, J. P. Kaye, C. F. Clark and J. P. Collins. 2011. Ecosystem response to nutrient enrichment across an urban airshed in the Sonoran Desert. Ecological Applications 21(3):640-660."}]}';


--
-- Name: plant_tissue_chn; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.plant_tissue_chn (
    id integer NOT NULL,
    "Run" text,
    "Run #" double precision,
    "Weight" double precision,
    "Created on" text,
    "Mode" text,
    "Comment" text,
    "Carbon %" double precision,
    "Hydrogen %" double precision,
    "Nitrogen %" double precision,
    "ZR" double precision,
    "CR" double precision,
    "HR" double precision,
    "NR" double precision,
    "Carbon" double precision,
    "Hydrogen" double precision,
    "Nitrogen" double precision,
    "Seconds" double precision,
    "Messages" text,
    plot_id integer,
    collection_date date,
    tissue_type text,
    source_file text,
    upload_batch numeric,
    omit boolean,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE urbancndep.plant_tissue_chn OWNER TO caplter;

--
-- Name: TABLE plant_tissue_chn; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON TABLE urbancndep.plant_tissue_chn IS 'this table houses the raw CHN data from plant tissue analyses';


--
-- Name: plant_tissue_chn_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.plant_tissue_chn_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.plant_tissue_chn_id_seq OWNER TO caplter;

--
-- Name: plant_tissue_chn_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.plant_tissue_chn_id_seq OWNED BY urbancndep.plant_tissue_chn.id;


--
-- Name: prs_probe_location_list_prsprobelocationlist_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.prs_probe_location_list_prsprobelocationlist_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.prs_probe_location_list_prsprobelocationlist_id_seq OWNER TO caplter;

--
-- Name: prs_probe_location_list; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.prs_probe_location_list (
    prsprobelocationlist_id integer DEFAULT nextval('urbancndep.prs_probe_location_list_prsprobelocationlist_id_seq'::regclass) NOT NULL,
    plot_id integer NOT NULL,
    "Date" date,
    "Location_IP" character varying(50),
    "Location_plant" character varying(50),
    notes character varying(255)
);


ALTER TABLE urbancndep.prs_probe_location_list OWNER TO caplter;

--
-- Name: COLUMN prs_probe_location_list.plot_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_probe_location_list.plot_id IS 'foriegn key reference to sites';


--
-- Name: COLUMN prs_probe_location_list."Date"; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_probe_location_list."Date" IS 'date of first PRS probe deployment';


--
-- Name: COLUMN prs_probe_location_list."Location_IP"; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_probe_location_list."Location_IP" IS 'location of PRS probes deployed in interplant space';


--
-- Name: COLUMN prs_probe_location_list."Location_plant"; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_probe_location_list."Location_plant" IS 'location of PRS probes deployed under Larrea tridentata plants';


--
-- Name: COLUMN prs_probe_location_list.notes; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.prs_probe_location_list.notes IS 'notes on conditions';


--
-- Name: resin; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.resin (
    id integer NOT NULL,
    upload_batch integer,
    field_id text,
    collection_date date,
    notes text,
    sample_id text,
    sample_type text,
    replicate_number double precision,
    repeat_number double precision,
    cup_number text,
    manual_dilution_factor double precision,
    auto_dilution_factor double precision,
    weight_units text,
    weight text,
    units text,
    detection_date text,
    detection_time text,
    user_name text,
    run_file_name text,
    description text,
    channel_number double precision,
    analyte_name text,
    peak_concentration double precision,
    determined_conc double precision,
    concentration_units text,
    peak_area double precision,
    peak_height double precision,
    calibration_equation text,
    retention_time double precision,
    inject_to_peak_start double precision,
    conc_x_adf double precision,
    conc_x_mdf double precision,
    conc_x_adf_x_mdf double precision,
    omit boolean,
    sourcefile text,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE urbancndep.resin OWNER TO caplter;

--
-- Name: TABLE resin; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON TABLE urbancndep.resin IS 'raw lachat output from analyses of resins data married to field sampling details';


--
-- Name: resin_analyses_analysis_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.resin_analyses_analysis_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.resin_analyses_analysis_id_seq OWNER TO caplter;

--
-- Name: resin_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.resin_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.resin_id_seq OWNER TO caplter;

--
-- Name: resin_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.resin_id_seq OWNED BY urbancndep.resin.id;


--
-- Name: sampling_events; Type: VIEW; Schema: urbancndep; Owner: caplter
--

CREATE VIEW urbancndep.sampling_events AS
 SELECT concat(stems.pre_date, '@', sites.code) AS id,
    stems.pre_date AS date,
    sites.id AS site_id,
    sites.code AS site_code,
    count(*) AS stems_counted
   FROM (((urbancndep.stems
     LEFT JOIN urbancndep.shrubs ON ((shrubs.id = stems.shrub_id)))
     LEFT JOIN urbancndep.plots ON ((shrubs.plot_id = plots.id)))
     LEFT JOIN urbancndep.sites ON ((plots.site_id = sites.id)))
  WHERE (1 = 1)
  GROUP BY stems.pre_date, sites.id, sites.code
  ORDER BY stems.pre_date, sites.id;


ALTER VIEW urbancndep.sampling_events OWNER TO caplter;

--
-- Name: sampling_runs_run_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.sampling_runs_run_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.sampling_runs_run_id_seq OWNER TO caplter;

--
-- Name: schema_migrations; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.schema_migrations (
    version character varying(255) NOT NULL
);


ALTER TABLE urbancndep.schema_migrations OWNER TO caplter;

--
-- Name: shrub_measurements_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.shrub_measurements_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.shrub_measurements_id_seq OWNER TO caplter;

--
-- Name: shrub_measurements; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.shrub_measurements (
    id integer DEFAULT nextval('urbancndep.shrub_measurements_id_seq'::regclass) NOT NULL,
    plot_id integer NOT NULL,
    survey_date date NOT NULL,
    plot_quadrant text,
    plant text NOT NULL,
    canopy_extent_n_s double precision,
    canopy_extent_e_w double precision,
    height double precision,
    "position_N" double precision,
    "position_E" double precision,
    notes text,
    height_by_distance double precision,
    height_by_degree_top double precision,
    height_by_degree_base double precision,
    created_at timestamp without time zone DEFAULT now(),
    updated_at timestamp without time zone DEFAULT now(),
    shrub_id integer
);


ALTER TABLE urbancndep.shrub_measurements OWNER TO caplter;

--
-- Name: COLUMN shrub_measurements.id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements.id IS 'auto incremented observation id';


--
-- Name: COLUMN shrub_measurements.plot_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements.plot_id IS 'CNdep study plot id #';


--
-- Name: COLUMN shrub_measurements.survey_date; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements.survey_date IS 'date measured';


--
-- Name: COLUMN shrub_measurements.plot_quadrant; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements.plot_quadrant IS 'quadrant of plot where measured plant is located';


--
-- Name: COLUMN shrub_measurements.plant; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements.plant IS 'identity of plant measured';


--
-- Name: COLUMN shrub_measurements.canopy_extent_n_s; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements.canopy_extent_n_s IS 'extent of canopy in N-S direction (m)';


--
-- Name: COLUMN shrub_measurements.canopy_extent_e_w; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements.canopy_extent_e_w IS 'extent of canopy in E-W direction (m)';


--
-- Name: COLUMN shrub_measurements.height; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements.height IS 'height of canopy (m)';


--
-- Name: COLUMN shrub_measurements."position_N"; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements."position_N" IS 'position of plant relative to SW corner of plot (meters north)';


--
-- Name: COLUMN shrub_measurements."position_E"; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements."position_E" IS 'position of plant relative to SW corner of plot (meters east)';


--
-- Name: COLUMN shrub_measurements.notes; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements.notes IS 'field notes';


--
-- Name: COLUMN shrub_measurements.height_by_distance; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements.height_by_distance IS 'distance (m) from base of plant if height measured by triangulation';


--
-- Name: COLUMN shrub_measurements.height_by_degree_top; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements.height_by_degree_top IS 'height of canopy (d)if height measured by triangulation';


--
-- Name: COLUMN shrub_measurements.height_by_degree_base; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.shrub_measurements.height_by_degree_base IS 'height of plant base (d) if height measured by triangulationheight of ';


--
-- Name: stem_comment; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.stem_comment (
    id integer NOT NULL,
    stem_id integer,
    post_measurement boolean,
    comment text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE urbancndep.stem_comment OWNER TO caplter;

--
-- Name: COLUMN stem_comment.id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_comment.id IS 'unique identifier';


--
-- Name: COLUMN stem_comment.stem_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_comment.stem_id IS 'foreign key to stems.stem_id';


--
-- Name: COLUMN stem_comment.post_measurement; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_comment.post_measurement IS 'f corresponds to pre, t to post';


--
-- Name: COLUMN stem_comment.comment; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_comment.comment IS 'comment supplied by field crew';


--
-- Name: stem_comment_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.stem_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.stem_comment_id_seq OWNER TO caplter;

--
-- Name: stem_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.stem_comment_id_seq OWNED BY urbancndep.stem_comment.id;


--
-- Name: stem_extra_datum_stem_extra_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.stem_extra_datum_stem_extra_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.stem_extra_datum_stem_extra_id_seq OWNER TO caplter;

--
-- Name: stem_extra_datum; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.stem_extra_datum (
    id integer DEFAULT nextval('urbancndep.stem_extra_datum_stem_extra_id_seq'::regclass) NOT NULL,
    stem_id integer NOT NULL,
    num_leaves integer,
    num_inflorescence integer,
    stem_diameter_in_mm double precision,
    created_at timestamp without time zone,
    updated_at timestamp without time zone,
    post_measurement boolean DEFAULT false NOT NULL
);


ALTER TABLE urbancndep.stem_extra_datum OWNER TO caplter;

--
-- Name: COLUMN stem_extra_datum.id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_extra_datum.id IS 'primary key, tracking additional and previous data from the old stems table';


--
-- Name: COLUMN stem_extra_datum.stem_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_extra_datum.stem_id IS 'foriegn key to stems table';


--
-- Name: COLUMN stem_extra_datum.num_leaves; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_extra_datum.num_leaves IS 'Historical data from the old stems table and database';


--
-- Name: COLUMN stem_extra_datum.num_inflorescence; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_extra_datum.num_inflorescence IS 'Historical data from the old stems table and database';


--
-- Name: COLUMN stem_extra_datum.stem_diameter_in_mm; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_extra_datum.stem_diameter_in_mm IS 'Historical data from the old stems table and database';


--
-- Name: COLUMN stem_extra_datum.post_measurement; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON COLUMN urbancndep.stem_extra_datum.post_measurement IS 'if this data was taken pre measurement (false) or post (true)';


--
-- Name: stem_plot_notes; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.stem_plot_notes (
    id integer NOT NULL,
    plot_id integer,
    survey_date date,
    plot_notes text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE urbancndep.stem_plot_notes OWNER TO caplter;

--
-- Name: stem_plot_notes_id_seq; Type: SEQUENCE; Schema: urbancndep; Owner: caplter
--

CREATE SEQUENCE urbancndep.stem_plot_notes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE urbancndep.stem_plot_notes_id_seq OWNER TO caplter;

--
-- Name: stem_plot_notes_id_seq; Type: SEQUENCE OWNED BY; Schema: urbancndep; Owner: caplter
--

ALTER SEQUENCE urbancndep.stem_plot_notes_id_seq OWNED BY urbancndep.stem_plot_notes.id;


--
-- Name: upload_urbancndepsoilph; Type: TABLE; Schema: urbancndep; Owner: caplter
--

CREATE TABLE urbancndep.upload_urbancndepsoilph (
    region character varying NOT NULL,
    site character varying NOT NULL,
    plot integer NOT NULL,
    tmt character varying,
    spec character varying NOT NULL,
    soil_ph real,
    tempeture real,
    slope character varying,
    "time" character varying,
    date date NOT NULL,
    extract_type character varying NOT NULL,
    ca_cl2_added character varying NOT NULL,
    staff_initials character varying NOT NULL,
    processing_label character varying NOT NULL,
    processing_notes character varying
);


ALTER TABLE urbancndep.upload_urbancndepsoilph OWNER TO caplter;

--
-- Name: active_admin_comments id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.active_admin_comments ALTER COLUMN id SET DEFAULT nextval('urbancndep.active_admin_comments_id_seq'::regclass);


--
-- Name: admin_users id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.admin_users ALTER COLUMN id SET DEFAULT nextval('urbancndep.admin_users_id_seq'::regclass);


--
-- Name: analysis_run_data id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.analysis_run_data ALTER COLUMN id SET DEFAULT nextval('urbancndep.analysis_run_data_id_seq'::regclass);


--
-- Name: analysis_runs run_id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.analysis_runs ALTER COLUMN run_id SET DEFAULT nextval('urbancndep.analysis_runs_run_id_seq'::regclass);


--
-- Name: annuals_biomass ann_biomass_id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.annuals_biomass ALTER COLUMN ann_biomass_id SET DEFAULT nextval('urbancndep.annuals_biomass_ann_biomass_id_seq'::regclass);


--
-- Name: cover_composition cover_id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_composition ALTER COLUMN cover_id SET DEFAULT nextval('urbancndep.cover_composition_cover_id_seq'::regclass);


--
-- Name: cover_events cover_event_id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_events ALTER COLUMN cover_event_id SET DEFAULT nextval('urbancndep.cover_events_cover_event_id_seq'::regclass);


--
-- Name: cover_types cover_type_id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_types ALTER COLUMN cover_type_id SET DEFAULT nextval('urbancndep.cover_types_cover_type_id_seq'::regclass);


--
-- Name: lachat_data id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.lachat_data ALTER COLUMN id SET DEFAULT nextval('urbancndep.lachat_data_id_seq'::regclass);


--
-- Name: plant_tissue_chn id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.plant_tissue_chn ALTER COLUMN id SET DEFAULT nextval('urbancndep.plant_tissue_chn_id_seq'::regclass);


--
-- Name: resin id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.resin ALTER COLUMN id SET DEFAULT nextval('urbancndep.resin_id_seq'::regclass);


--
-- Name: stem_comment id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.stem_comment ALTER COLUMN id SET DEFAULT nextval('urbancndep.stem_comment_id_seq'::regclass);


--
-- Name: stem_plot_notes id; Type: DEFAULT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.stem_plot_notes ALTER COLUMN id SET DEFAULT nextval('urbancndep.stem_plot_notes_id_seq'::regclass);


--
-- Name: active_admin_comments active_admin_comments_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.active_admin_comments
    ADD CONSTRAINT active_admin_comments_pkey PRIMARY KEY (id);


--
-- Name: addtl_site_info addtl_site_info_add_site_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.addtl_site_info
    ADD CONSTRAINT addtl_site_info_add_site_id_pkey PRIMARY KEY (add_site_id);


--
-- Name: admin_users admin_users_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.admin_users
    ADD CONSTRAINT admin_users_pkey PRIMARY KEY (id);


--
-- Name: analysis analysis_analysis_test_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.analysis
    ADD CONSTRAINT analysis_analysis_test_id_pkey PRIMARY KEY (analysis_test_id);


--
-- Name: analysis_run_data analysis_run_data_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.analysis_run_data
    ADD CONSTRAINT analysis_run_data_pkey PRIMARY KEY (id);


--
-- Name: analysis_runs analysis_run_primary_key; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.analysis_runs
    ADD CONSTRAINT analysis_run_primary_key PRIMARY KEY (run_id);


--
-- Name: annuals_biomass annuals_biomass_pk; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.annuals_biomass
    ADD CONSTRAINT annuals_biomass_pk PRIMARY KEY (ann_biomass_id);


--
-- Name: annuals_biomass annuals_biomass_unique_observations; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.annuals_biomass
    ADD CONSTRAINT annuals_biomass_unique_observations UNIQUE (plot_id, location_within_plot, replicate, subquad_orientation, year);


--
-- Name: chn_plant_analysis chn_plant_analysis_chn_analysis_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.chn_plant_analysis
    ADD CONSTRAINT chn_plant_analysis_chn_analysis_id_pkey PRIMARY KEY (chn_analysis_id);


--
-- Name: cover_composition cover_composition_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_composition
    ADD CONSTRAINT cover_composition_pkey PRIMARY KEY (cover_id);


--
-- Name: cover_composition cover_composition_unique_event_type; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_composition
    ADD CONSTRAINT cover_composition_unique_event_type UNIQUE (cover_event_id, cover_type_id);


--
-- Name: cover_events cover_events_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_events
    ADD CONSTRAINT cover_events_pkey PRIMARY KEY (cover_event_id);


--
-- Name: cover_events cover_events_unique_event_year; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_events
    ADD CONSTRAINT cover_events_unique_event_year UNIQUE (plot, patch_type, subplot, year);


--
-- Name: cover_types cover_types_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_types
    ADD CONSTRAINT cover_types_pkey PRIMARY KEY (cover_type_id);


--
-- Name: cover_types cover_types_unique_cover_type; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_types
    ADD CONSTRAINT cover_types_unique_cover_type UNIQUE (cover_type);


--
-- Name: lachat_data csvs_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.lachat_data
    ADD CONSTRAINT csvs_pkey PRIMARY KEY (id);


--
-- Name: fertilizer_applications fertilizer_application_application_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.fertilizer_applications
    ADD CONSTRAINT fertilizer_application_application_id_pkey PRIMARY KEY (id);


--
-- Name: fertilizer_applications fertilizer_applications_site_id_date_key; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.fertilizer_applications
    ADD CONSTRAINT fertilizer_applications_site_id_date_key UNIQUE (site_id, date);


--
-- Name: lachat_data lachat_data_hash_key; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.lachat_data
    ADD CONSTRAINT lachat_data_hash_key UNIQUE (file_digest);


--
-- Name: stems new_stems_stem_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.stems
    ADD CONSTRAINT new_stems_stem_id_pkey PRIMARY KEY (id);


--
-- Name: plots plots_plot_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.plots
    ADD CONSTRAINT plots_plot_id_pkey PRIMARY KEY (id);


--
-- Name: prs_analysis prs_analysis_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.prs_analysis
    ADD CONSTRAINT prs_analysis_id_pkey PRIMARY KEY (id);


--
-- Name: prs_probe_location_list prs_probe_location_list_prsprobelocationlist_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.prs_probe_location_list
    ADD CONSTRAINT prs_probe_location_list_prsprobelocationlist_id_pkey PRIMARY KEY (prsprobelocationlist_id);


--
-- Name: resin_locations resin_locations_location_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.resin_locations
    ADD CONSTRAINT resin_locations_location_id_pkey PRIMARY KEY (location_id);


--
-- Name: resin resin_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.resin
    ADD CONSTRAINT resin_pkey PRIMARY KEY (id);


--
-- Name: shrub_measurements shrubmeasurments_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.shrub_measurements
    ADD CONSTRAINT shrubmeasurments_id_pkey PRIMARY KEY (id);


--
-- Name: shrubs shrubs_shrub_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.shrubs
    ADD CONSTRAINT shrubs_shrub_id_pkey PRIMARY KEY (id);


--
-- Name: sites sites_site_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.sites
    ADD CONSTRAINT sites_site_id_pkey PRIMARY KEY (id);


--
-- Name: soil_ph_samples soil_ph_sample_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.soil_ph_samples
    ADD CONSTRAINT soil_ph_sample_id_pkey PRIMARY KEY (id);


--
-- Name: shrub_species species_lookup_species_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.shrub_species
    ADD CONSTRAINT species_lookup_species_id_pkey PRIMARY KEY (id);


--
-- Name: stem_comment stem_comment_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.stem_comment
    ADD CONSTRAINT stem_comment_id_pkey PRIMARY KEY (id);


--
-- Name: stem_extra_datum stem_extra_data_stem_extra_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.stem_extra_datum
    ADD CONSTRAINT stem_extra_data_stem_extra_id_pkey PRIMARY KEY (id);


--
-- Name: archived_table_stem_length_ambrosia stem_length_ambrosia_stem_length_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.archived_table_stem_length_ambrosia
    ADD CONSTRAINT stem_length_ambrosia_stem_length_id_pkey PRIMARY KEY (stem_length_id);


--
-- Name: stem_lengths stem_length_stem_length_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.stem_lengths
    ADD CONSTRAINT stem_length_stem_length_id_pkey PRIMARY KEY (id);


--
-- Name: archived_table_stem_observations stem_observations_stem_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.archived_table_stem_observations
    ADD CONSTRAINT stem_observations_stem_id_pkey PRIMARY KEY (stem_id);


--
-- Name: stem_plot_notes stem_plot_notes_pkey_id; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.stem_plot_notes
    ADD CONSTRAINT stem_plot_notes_pkey_id PRIMARY KEY (id);


--
-- Name: archived_table_stems_ambrosia stems_ambrosia_stem_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.archived_table_stems_ambrosia
    ADD CONSTRAINT stems_ambrosia_stem_id_pkey PRIMARY KEY (stem_id);


--
-- Name: treatments treatments_treatment_id_pkey; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.treatments
    ADD CONSTRAINT treatments_treatment_id_pkey PRIMARY KEY (id);


--
-- Name: cover_events unique_cover_events_year_plot_patch_type_subplot; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_events
    ADD CONSTRAINT unique_cover_events_year_plot_patch_type_subplot UNIQUE (year, plot, patch_type, subplot);


--
-- Name: plant_tissue_chn unique_plant_tissue_chn_plot_id_collection_date_tissue_type_omi; Type: CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.plant_tissue_chn
    ADD CONSTRAINT unique_plant_tissue_chn_plot_id_collection_date_tissue_type_omi UNIQUE (plot_id, collection_date, tissue_type, omit);


--
-- Name: addtl_site_info_site_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX addtl_site_info_site_id ON urbancndep.addtl_site_info USING btree (site_id);


--
-- Name: analysis_analysis; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX analysis_analysis ON urbancndep.analysis USING btree (analysis);


--
-- Name: chn_plant_analysis_analysis_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX chn_plant_analysis_analysis_id ON urbancndep.chn_plant_analysis USING btree (analysis_id);


--
-- Name: chn_plant_analysis_plot_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX chn_plant_analysis_plot_id ON urbancndep.chn_plant_analysis USING btree (plot_id);


--
-- Name: fertilizer_application_site_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX fertilizer_application_site_id ON urbancndep.fertilizer_applications USING btree (site_id);


--
-- Name: fki_analysis_run_data_analysis_run_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX fki_analysis_run_data_analysis_run_id ON urbancndep.analysis_run_data USING btree (analysis_run_id);


--
-- Name: index_active_admin_comments_on_author_type_and_author_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX index_active_admin_comments_on_author_type_and_author_id ON urbancndep.active_admin_comments USING btree (author_type, author_id);


--
-- Name: index_active_admin_comments_on_namespace; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX index_active_admin_comments_on_namespace ON urbancndep.active_admin_comments USING btree (namespace);


--
-- Name: index_active_admin_comments_on_resource_type_and_resource_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX index_active_admin_comments_on_resource_type_and_resource_id ON urbancndep.active_admin_comments USING btree (resource_type, resource_id);


--
-- Name: index_admin_users_on_email; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE UNIQUE INDEX index_admin_users_on_email ON urbancndep.admin_users USING btree (email);


--
-- Name: index_admin_users_on_reset_password_token; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE UNIQUE INDEX index_admin_users_on_reset_password_token ON urbancndep.admin_users USING btree (reset_password_token);


--
-- Name: index_admin_users_on_username; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE UNIQUE INDEX index_admin_users_on_username ON urbancndep.admin_users USING btree (username);


--
-- Name: index_analysis_run_data_on_lachat_data_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX index_analysis_run_data_on_lachat_data_id ON urbancndep.analysis_run_data USING btree (lachat_data_id);


--
-- Name: index_analysis_runs_on_lachat_data_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX index_analysis_runs_on_lachat_data_id ON urbancndep.analysis_runs USING btree (lachat_data_id);


--
-- Name: index_lachat_data_on_name; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX index_lachat_data_on_name ON urbancndep.lachat_data USING btree (name);


--
-- Name: new_stems_shrub_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX new_stems_shrub_id ON urbancndep.stems USING btree (shrub_id);


--
-- Name: plots_site_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX plots_site_id ON urbancndep.plots USING btree (site_id);


--
-- Name: plots_treatment_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX plots_treatment_id ON urbancndep.plots USING btree (treatment_id);


--
-- Name: prs_analysis_plot_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX prs_analysis_plot_id ON urbancndep.prs_analysis USING btree (plot_id);


--
-- Name: prs_analysis_wal_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX prs_analysis_wal_id ON urbancndep.prs_analysis USING btree (wal_id);


--
-- Name: prs_probe_location_list_plot_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX prs_probe_location_list_plot_id ON urbancndep.prs_probe_location_list USING btree (plot_id);


--
-- Name: resin_field_date_replicate_analyte_key; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE UNIQUE INDEX resin_field_date_replicate_analyte_key ON urbancndep.resin USING btree (field_id, collection_date, replicate_number, analyte_name) WHERE ((sample_type ~~* 'unknown'::text) AND (field_id IS NOT NULL) AND (field_id !~~* '%blk%'::text) AND (omit = false));


--
-- Name: resin_locations_location_code; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX resin_locations_location_code ON urbancndep.resin_locations USING btree (location_code);


--
-- Name: shrubmeasurments_plot_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX shrubmeasurments_plot_id ON urbancndep.shrub_measurements USING btree (plot_id);


--
-- Name: shrubs_plot_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX shrubs_plot_id ON urbancndep.shrubs USING btree (plot_id);


--
-- Name: shrubs_species_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX shrubs_species_id ON urbancndep.shrubs USING btree (shrub_species_id);


--
-- Name: site_id_lachat_data_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE UNIQUE INDEX site_id_lachat_data_id ON urbancndep.analysis_runs USING btree (site_id, lachat_data_id);


--
-- Name: INDEX site_id_lachat_data_id; Type: COMMENT; Schema: urbancndep; Owner: caplter
--

COMMENT ON INDEX urbancndep.site_id_lachat_data_id IS 'unique index for site_lachat data, which means single site analysis run for lachat data file';


--
-- Name: sites_site_code; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX sites_site_code ON urbancndep.sites USING btree (code);


--
-- Name: soil_ph_plot_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX soil_ph_plot_id ON urbancndep.soil_ph_samples USING btree (plot_id);


--
-- Name: soil_ph_species_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX soil_ph_species_id ON urbancndep.soil_ph_samples USING btree (location_id);


--
-- Name: species_lookup_species_code; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX species_lookup_species_code ON urbancndep.shrub_species USING btree (code);


--
-- Name: stem_extra_data_stem_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX stem_extra_data_stem_id ON urbancndep.stem_extra_datum USING btree (stem_id);


--
-- Name: stem_length_ambrosia_stem_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX stem_length_ambrosia_stem_id ON urbancndep.archived_table_stem_length_ambrosia USING btree (stem_id);


--
-- Name: stem_length_stem_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX stem_length_stem_id ON urbancndep.stem_lengths USING btree (stem_id);


--
-- Name: stem_observations_plot_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX stem_observations_plot_id ON urbancndep.archived_table_stem_observations USING btree (plot_id);


--
-- Name: stem_observations_sample_period_shrub_id_stem_direction; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE UNIQUE INDEX stem_observations_sample_period_shrub_id_stem_direction ON urbancndep.archived_table_stem_observations USING btree (sample_period, shrub_id, stem_direction);


--
-- Name: stem_observations_shrub_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX stem_observations_shrub_id ON urbancndep.archived_table_stem_observations USING btree (shrub_id);


--
-- Name: stem_observations_species_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX stem_observations_species_id ON urbancndep.archived_table_stem_observations USING btree (species_id);


--
-- Name: stems_ambrosia_plot_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX stems_ambrosia_plot_id ON urbancndep.archived_table_stems_ambrosia USING btree (plot_id);


--
-- Name: stems_ambrosia_species_id; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE INDEX stems_ambrosia_species_id ON urbancndep.archived_table_stems_ambrosia USING btree (species_id);


--
-- Name: treatments_treatment_code; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE UNIQUE INDEX treatments_treatment_code ON urbancndep.treatments USING btree (code);


--
-- Name: unique_schema_migrations; Type: INDEX; Schema: urbancndep; Owner: caplter
--

CREATE UNIQUE INDEX unique_schema_migrations ON urbancndep.schema_migrations USING btree (version);


--
-- Name: annuals_biomass set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.annuals_biomass FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: cover_composition set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.cover_composition FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: cover_events set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.cover_events FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: cover_types set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.cover_types FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: fertilizer_applications set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.fertilizer_applications FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: plant_tissue_chn set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.plant_tissue_chn FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: prs_analysis set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.prs_analysis FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: resin set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.resin FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: shrub_measurements set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.shrub_measurements FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: shrubs set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.shrubs FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: stem_comment set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.stem_comment FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: stem_lengths set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.stem_lengths FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: stem_plot_notes set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.stem_plot_notes FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: stems set_timestamp; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_timestamp BEFORE UPDATE ON urbancndep.stems FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: active_admin_comments set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.active_admin_comments FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: admin_users set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.admin_users FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: analysis_run_data set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.analysis_run_data FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: annuals_biomass set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.annuals_biomass FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: cover_composition set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.cover_composition FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: cover_events set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.cover_events FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: cover_types set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.cover_types FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: fertilizer_applications set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.fertilizer_applications FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: lachat_data set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.lachat_data FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: plant_tissue_chn set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.plant_tissue_chn FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: plots set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.plots FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: prs_analysis set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.prs_analysis FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: resin set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.resin FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: shrub_measurements set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.shrub_measurements FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: shrubs set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.shrubs FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: sites set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.sites FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: soil_ph_samples set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.soil_ph_samples FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: stem_comment set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.stem_comment FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: stem_extra_datum set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.stem_extra_datum FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: stem_lengths set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.stem_lengths FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: stem_plot_notes set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.stem_plot_notes FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: stems set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.stems FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: treatments set_updated_at; Type: TRIGGER; Schema: urbancndep; Owner: caplter
--

CREATE TRIGGER set_updated_at BEFORE UPDATE ON urbancndep.treatments FOR EACH ROW EXECUTE FUNCTION urbancndep.trigger_set_timestamp();


--
-- Name: addtl_site_info addtl_site_info_site_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.addtl_site_info
    ADD CONSTRAINT addtl_site_info_site_id_fkey FOREIGN KEY (site_id) REFERENCES urbancndep.sites(id);


--
-- Name: analysis_run_data analysis_run_data_analysis_run_id; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.analysis_run_data
    ADD CONSTRAINT analysis_run_data_analysis_run_id FOREIGN KEY (analysis_run_id) REFERENCES urbancndep.analysis_runs(run_id);


--
-- Name: analysis_run_data analysis_run_data_lachat_data; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.analysis_run_data
    ADD CONSTRAINT analysis_run_data_lachat_data FOREIGN KEY (lachat_data_id) REFERENCES urbancndep.lachat_data(id);


--
-- Name: analysis_runs analysis_run_site_id; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.analysis_runs
    ADD CONSTRAINT analysis_run_site_id FOREIGN KEY (site_id) REFERENCES urbancndep.sites(id);


--
-- Name: annuals_biomass annuals_biomass_fk_plot_id; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.annuals_biomass
    ADD CONSTRAINT annuals_biomass_fk_plot_id FOREIGN KEY (plot_id) REFERENCES urbancndep.plots(id);


--
-- Name: chn_plant_analysis chn_plant_analysis_analysis_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.chn_plant_analysis
    ADD CONSTRAINT chn_plant_analysis_analysis_id_fkey FOREIGN KEY (analysis_id) REFERENCES urbancndep.analysis(analysis_test_id);


--
-- Name: chn_plant_analysis chn_plant_analysis_plot_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.chn_plant_analysis
    ADD CONSTRAINT chn_plant_analysis_plot_id_fkey FOREIGN KEY (plot_id) REFERENCES urbancndep.plots(id);


--
-- Name: cover_composition cover_composition_fk_cover_event_id; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_composition
    ADD CONSTRAINT cover_composition_fk_cover_event_id FOREIGN KEY (cover_event_id) REFERENCES urbancndep.cover_events(cover_event_id);


--
-- Name: cover_composition cover_composition_fk_cover_type_id; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_composition
    ADD CONSTRAINT cover_composition_fk_cover_type_id FOREIGN KEY (cover_type_id) REFERENCES urbancndep.cover_types(cover_type_id);


--
-- Name: cover_events cover_events_fk_plot_id; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.cover_events
    ADD CONSTRAINT cover_events_fk_plot_id FOREIGN KEY (plot) REFERENCES urbancndep.plots(id);


--
-- Name: fertilizer_applications fertilizer_application_site_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.fertilizer_applications
    ADD CONSTRAINT fertilizer_application_site_id_fkey FOREIGN KEY (site_id) REFERENCES urbancndep.sites(id);


--
-- Name: stems new_stems_shrub_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.stems
    ADD CONSTRAINT new_stems_shrub_id_fkey FOREIGN KEY (shrub_id) REFERENCES urbancndep.shrubs(id);


--
-- Name: plots plots_site_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.plots
    ADD CONSTRAINT plots_site_id_fkey FOREIGN KEY (site_id) REFERENCES urbancndep.sites(id);


--
-- Name: plots plots_treatment_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.plots
    ADD CONSTRAINT plots_treatment_id_fkey FOREIGN KEY (treatment_id) REFERENCES urbancndep.treatments(id);


--
-- Name: prs_analysis prs_analysis_plot_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.prs_analysis
    ADD CONSTRAINT prs_analysis_plot_id_fkey FOREIGN KEY (plot_id) REFERENCES urbancndep.plots(id);


--
-- Name: prs_probe_location_list prs_probe_location_list_plot_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.prs_probe_location_list
    ADD CONSTRAINT prs_probe_location_list_plot_id_fkey FOREIGN KEY (plot_id) REFERENCES urbancndep.plots(id);


--
-- Name: shrub_measurements shrub_measurements_fk_shrub_id; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.shrub_measurements
    ADD CONSTRAINT shrub_measurements_fk_shrub_id FOREIGN KEY (shrub_id) REFERENCES urbancndep.shrubs(id);


--
-- Name: shrub_measurements shrubmeasurments_plot_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.shrub_measurements
    ADD CONSTRAINT shrubmeasurments_plot_id_fkey FOREIGN KEY (plot_id) REFERENCES urbancndep.plots(id);


--
-- Name: shrubs shrubs_plot_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.shrubs
    ADD CONSTRAINT shrubs_plot_id_fkey FOREIGN KEY (plot_id) REFERENCES urbancndep.plots(id);


--
-- Name: shrubs shrubs_species_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.shrubs
    ADD CONSTRAINT shrubs_species_id_fkey FOREIGN KEY (shrub_species_id) REFERENCES urbancndep.shrub_species(id);


--
-- Name: soil_ph_samples soil_ph_plot_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.soil_ph_samples
    ADD CONSTRAINT soil_ph_plot_id_fkey FOREIGN KEY (plot_id) REFERENCES urbancndep.plots(id);


--
-- Name: soil_ph_samples soil_ph_species_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.soil_ph_samples
    ADD CONSTRAINT soil_ph_species_id_fkey FOREIGN KEY (location_id) REFERENCES urbancndep.resin_locations(location_id);


--
-- Name: stem_comment stem_comment_stem_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.stem_comment
    ADD CONSTRAINT stem_comment_stem_id_fkey FOREIGN KEY (stem_id) REFERENCES urbancndep.stems(id);


--
-- Name: stem_extra_datum stem_extra_data_stem_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.stem_extra_datum
    ADD CONSTRAINT stem_extra_data_stem_id_fkey FOREIGN KEY (stem_id) REFERENCES urbancndep.stems(id);


--
-- Name: stem_lengths stem_length_stem_id_fkey; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.stem_lengths
    ADD CONSTRAINT stem_length_stem_id_fkey FOREIGN KEY (stem_id) REFERENCES urbancndep.stems(id);


--
-- Name: stem_plot_notes stem_plot_notes_fk_plot_id; Type: FK CONSTRAINT; Schema: urbancndep; Owner: caplter
--

ALTER TABLE ONLY urbancndep.stem_plot_notes
    ADD CONSTRAINT stem_plot_notes_fk_plot_id FOREIGN KEY (plot_id) REFERENCES urbancndep.plots(id);


--
-- Name: SCHEMA urbancndep; Type: ACL; Schema: -; Owner: caplter
--

GRANT ALL ON SCHEMA urbancndep TO shiny;


--
-- Name: TABLE active_admin_comments; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.active_admin_comments TO shiny;


--
-- Name: SEQUENCE active_admin_comments_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.active_admin_comments_id_seq TO shiny;


--
-- Name: SEQUENCE addtl_site_info_add_site_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.addtl_site_info_add_site_id_seq TO shiny;


--
-- Name: TABLE addtl_site_info; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.addtl_site_info TO shiny;


--
-- Name: TABLE admin_users; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.admin_users TO shiny;


--
-- Name: SEQUENCE admin_users_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.admin_users_id_seq TO shiny;


--
-- Name: SEQUENCE analysis_analysis_test_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.analysis_analysis_test_id_seq TO shiny;


--
-- Name: TABLE analysis; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.analysis TO shiny;


--
-- Name: TABLE analysis_run_data; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.analysis_run_data TO shiny;


--
-- Name: SEQUENCE analysis_run_data_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.analysis_run_data_id_seq TO shiny;


--
-- Name: TABLE analysis_runs; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.analysis_runs TO shiny;


--
-- Name: SEQUENCE analysis_runs_run_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.analysis_runs_run_id_seq TO shiny;


--
-- Name: TABLE annuals_biomass; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.annuals_biomass TO shiny;


--
-- Name: SEQUENCE annuals_biomass_ann_biomass_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.annuals_biomass_ann_biomass_id_seq TO shiny;


--
-- Name: SEQUENCE archived_table_stem_length_ambrosia_stem_length_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.archived_table_stem_length_ambrosia_stem_length_id_seq TO shiny;


--
-- Name: TABLE archived_table_stem_length_ambrosia; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.archived_table_stem_length_ambrosia TO shiny;


--
-- Name: SEQUENCE archived_table_stem_observations_stem_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.archived_table_stem_observations_stem_id_seq TO shiny;


--
-- Name: TABLE archived_table_stem_observations; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.archived_table_stem_observations TO shiny;


--
-- Name: SEQUENCE archived_table_stems_ambrosia_stem_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.archived_table_stems_ambrosia_stem_id_seq TO shiny;


--
-- Name: TABLE archived_table_stems_ambrosia; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.archived_table_stems_ambrosia TO shiny;


--
-- Name: SEQUENCE chn_plant_analysis_chn_analysis_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.chn_plant_analysis_chn_analysis_id_seq TO shiny;


--
-- Name: TABLE chn_plant_analysis; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.chn_plant_analysis TO shiny;


--
-- Name: TABLE cover_composition; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.cover_composition TO shiny;


--
-- Name: SEQUENCE cover_composition_cover_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.cover_composition_cover_id_seq TO shiny;


--
-- Name: TABLE cover_events; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.cover_events TO shiny;


--
-- Name: SEQUENCE cover_events_cover_event_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.cover_events_cover_event_id_seq TO shiny;


--
-- Name: TABLE cover_types; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.cover_types TO shiny;


--
-- Name: SEQUENCE cover_types_cover_type_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.cover_types_cover_type_id_seq TO shiny;


--
-- Name: SEQUENCE fertilizer_applications_application_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.fertilizer_applications_application_id_seq TO shiny;


--
-- Name: TABLE fertilizer_applications; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.fertilizer_applications TO shiny;


--
-- Name: TABLE lachat_data; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.lachat_data TO shiny;


--
-- Name: SEQUENCE lachat_data_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.lachat_data_id_seq TO shiny;


--
-- Name: SEQUENCE lachat_output_machine_analysis_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.lachat_output_machine_analysis_id_seq TO shiny;


--
-- Name: SEQUENCE sites_site_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.sites_site_id_seq TO shiny;


--
-- Name: TABLE sites; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.sites TO shiny;


--
-- Name: TABLE lens_fertilizer_applications; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.lens_fertilizer_applications TO shiny;


--
-- Name: SEQUENCE plots_plot_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.plots_plot_id_seq TO shiny;


--
-- Name: TABLE plots; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.plots TO shiny;


--
-- Name: SEQUENCE shrub_species_species_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.shrub_species_species_id_seq TO shiny;


--
-- Name: TABLE shrub_species; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.shrub_species TO shiny;


--
-- Name: SEQUENCE shrubs_shrub_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.shrubs_shrub_id_seq TO shiny;


--
-- Name: TABLE shrubs; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.shrubs TO shiny;


--
-- Name: SEQUENCE stem_lengths_stem_length_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.stem_lengths_stem_length_id_seq TO shiny;


--
-- Name: TABLE stem_lengths; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.stem_lengths TO shiny;


--
-- Name: SEQUENCE stems_stem_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.stems_stem_id_seq TO shiny;


--
-- Name: TABLE stems; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.stems TO shiny;


--
-- Name: SEQUENCE treatments_treatment_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.treatments_treatment_id_seq TO shiny;


--
-- Name: TABLE treatments; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.treatments TO shiny;


--
-- Name: TABLE lens_plant_growth; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.lens_plant_growth TO shiny;


--
-- Name: TABLE lens_plant_growth_ambrosia; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.lens_plant_growth_ambrosia TO shiny;


--
-- Name: SEQUENCE prs_analysis_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.prs_analysis_id_seq TO shiny;


--
-- Name: TABLE prs_analysis; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.prs_analysis TO shiny;


--
-- Name: TABLE lens_prs_probe_analysis; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.lens_prs_probe_analysis TO shiny;


--
-- Name: SEQUENCE resin_locations_location_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.resin_locations_location_id_seq TO shiny;


--
-- Name: TABLE resin_locations; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.resin_locations TO shiny;


--
-- Name: SEQUENCE soil_ph_samples_sample_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.soil_ph_samples_sample_id_seq TO shiny;


--
-- Name: TABLE soil_ph_samples; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.soil_ph_samples TO shiny;


--
-- Name: TABLE lens_soil_ph; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.lens_soil_ph TO shiny;


--
-- Name: TABLE plant_tissue_chn; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.plant_tissue_chn TO shiny;


--
-- Name: SEQUENCE plant_tissue_chn_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.plant_tissue_chn_id_seq TO shiny;


--
-- Name: SEQUENCE prs_probe_location_list_prsprobelocationlist_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.prs_probe_location_list_prsprobelocationlist_id_seq TO shiny;


--
-- Name: TABLE prs_probe_location_list; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.prs_probe_location_list TO shiny;


--
-- Name: TABLE resin; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.resin TO shiny;


--
-- Name: SEQUENCE resin_analyses_analysis_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.resin_analyses_analysis_id_seq TO shiny;


--
-- Name: SEQUENCE resin_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.resin_id_seq TO shiny;


--
-- Name: TABLE sampling_events; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.sampling_events TO shiny;


--
-- Name: SEQUENCE sampling_runs_run_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.sampling_runs_run_id_seq TO shiny;


--
-- Name: TABLE schema_migrations; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.schema_migrations TO shiny;


--
-- Name: SEQUENCE shrub_measurements_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.shrub_measurements_id_seq TO shiny;


--
-- Name: TABLE shrub_measurements; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.shrub_measurements TO shiny;


--
-- Name: TABLE stem_comment; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.stem_comment TO shiny;


--
-- Name: SEQUENCE stem_comment_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.stem_comment_id_seq TO shiny;


--
-- Name: SEQUENCE stem_extra_datum_stem_extra_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.stem_extra_datum_stem_extra_id_seq TO shiny;


--
-- Name: TABLE stem_extra_datum; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.stem_extra_datum TO shiny;


--
-- Name: TABLE stem_plot_notes; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.stem_plot_notes TO shiny;


--
-- Name: SEQUENCE stem_plot_notes_id_seq; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON SEQUENCE urbancndep.stem_plot_notes_id_seq TO shiny;


--
-- Name: TABLE upload_urbancndepsoilph; Type: ACL; Schema: urbancndep; Owner: caplter
--

GRANT ALL ON TABLE urbancndep.upload_urbancndepsoilph TO shiny;


--
-- PostgreSQL database dump complete
--

\unrestrict Jl2UwL7cdiGlFaEAwFjb4dvDWHZxYbE9IhRXPPJA9rEgZT9cUa6OfJeqOrOFYcF

