*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

DEFINE eat_white.
  while_offset_cs sv_white_space.
END-OF-DEFINITION.

DEFINE while_offset_cs.
*  >= 7.02 alternative
*  pos = find_any_not_of( val = json sub = &1 off = offset ).
*  if pos eq -1. offset = length.
*  else. offset = pos. endif.

* < 7.02
  while offset < length.
    find first occurrence of json+offset(1) in &1.
    if sy-subrc is not initial.
      exit.
    endif.
    offset = offset + 1.
  endwhile.
* < 7.02

END-OF-DEFINITION.
