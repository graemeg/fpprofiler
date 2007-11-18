function fpprof_getsystemtime: QWord;
begin
  Result := Round(Now * 24 * 3600 * 1000);
end;

