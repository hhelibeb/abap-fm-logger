[中文](./README.zh-CN.md) | **English**

# abap-fm-logger
A logger for function module (RFC)

Let's work with function modules like IDoc.

ABAP Version: 740 or higher

## Design Goals
- Save FM logs in a single general table.
- Store parameters in JSON format.
- Enable reprocessing of records using log id, akin to WE19.
- Search logs by FM & field value, akin to WE10.

## Log Report
The report ```ZAFL_VIEWER``` empowers you to search logs and reprocess specific records.
![log report](https://github.com/hhelibeb/helloworld/blob/master/log%20report.png)

## How to Use

1, Add the include file ```ZAFL_MACROS``` which contains core function of the logger.
```abap
FUNCTION-POOL zzxxxx.
INCLUDE zafl_macros.
```
2, Call the macros on demand in the function.
```abap
FUNCTION z_fm.
  
**initialize logger. It should be always on the top of the FUNCTION.
  /afl/log_init.

**optional, you can specify at most 3 fields for search.
  /afl/set_custom_fields '2020' '1001' '2000000001'. 

**optional, you can save a status code and message text for search.
  /afl/set_status 'S' 'message'.

**save logs. It should be called before every exit point of the FUNCTION.
  /afl/save.

ENDFUNCTION.
```

Tip: to avoid calling `/afl/save` at every exit point when the real logic has
multiple returns, you can sink the business logic into a separate internal FM
(e.g. `Z_FM_INT`) and keep the logged FM as a single-exit shell that calls it
— then one `/afl/save` at the end suffices.

## Configuration
Table ```ZAFL_CONFIG```  allows for basic configuration.

### Basic Settings
- FNAME: Name of Function Module
- ENABLED: If checked, log function will be enabled.
- EXPORT: If checked, logs for export parameters will be enabled.
- IMPORT: If checked, logs for import parameters will be enabled.
- TABLE_IN: If checked, logs for tables parameters (at the start of the FM) will be enabled.
- TABLE_OUT: If checked, logs for tables parameters (at the end of the FM) will be enabled.
- CHANGE: If checked, logs for changing parameters (both in and out) will be enabled.

### Additional Settings
- NO_COMMIT: If checked, the COMMIT statement in ```/afl/save``` will be bypassed.
- NO_AUTH_CHECK: If checked, the authority check statement in the reprocess method will be bypassed.

## Log Cleaner
Schedule the report ```ZAFL_HISTORY_CLEANER``` as a job to delete history log.

## ⚠️ Upgrade Reminder
If you have installed a previous version, when installing a new version please check whether your installed table `ZAFL_LOG` differs from the latest version. Differences may arise from: 1) you have manually modified the local `ZAFL_LOG`, or 2) the new version has extended new fields or widened existing field lengths. If differences exist, please manually review the changes to avoid data issues.
