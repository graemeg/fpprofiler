function fpprof_getsystemtime: QWord;
begin
  fpprof_getsystemtime := Round(Now * 24 * 3600 * 1000);
end;

