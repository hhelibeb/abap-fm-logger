# abap-fm-logger
A logger for function module (RFC)

ABAP Version: 740 or higher

## Design Goals
- Save FM logs in ONE general table.
- Save parameters in json format.
- Reprocess records by log id (like WE19).
- Search logs by FM & field value (like WE10).

## Log Report
Report ```ZAFL_VIEWER``` allow you search logs and reprocess specified records.
![log report](https://github.com/hhelibeb/helloworld/blob/master/log%20report.png)

## Usage

1, Add the include file ```ZAFL_MACROS``` which contains core function of the logger.
```abap
FUNCTION-POOL zzxxxx.
INCLUDE zafl_macros.
```
2, Call the macros on demand in the function.
```abap
FUNCTION zib_iqs_subassembly_get_test.
  
**initialize logger. It should be always on the top of the FUNCTION.
  /afl/log_init.

**optional, you can specify at most 3 fields for search.
  /afl/set_custom_fields 'cust field1' 'cust field2' 'cust field3'. 

**optional, you can save a status code and message text for search.
  /afl/set_status 'S' 'message'.

**save logs. It should be always on the bottom of the FUNCTION.
  /afl/save.

ENDFUNCTION.
```
## Configuration
Table ZAFL_CONFIG allows you do some basic configuration.

- FNAME: Name of Function Module
- ENABLED: If checked, enable log function.
- EXPORT: If checked, enable log for export parameters.
- IMPORT: If checked, enable log for import parameters..
