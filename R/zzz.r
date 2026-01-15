# R/zzz.R
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n╔══════════════════════════════════════════════════════╗",
    "\n║       MS2 Database Builder v1.0                     ║",
    "\n║       Type runMS2DatabaseBuilder() to start         ║",
    "\n╚══════════════════════════════════════════════════════╝",
    "\n"
  )
}

.onLoad <- function(libname, pkgname) {
  # 包加载时的初始化代码
  invisible()
}