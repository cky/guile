/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ guile / guile /
  tb
  s/ $/ guile /
  :b
  s/^/# Packages using this file:/
}
