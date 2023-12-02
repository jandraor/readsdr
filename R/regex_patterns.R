get_pattern_regex <- function(pattern) {

  if(pattern == "net_flow") return ("net_flow\\((.+?)\\)")

  if(pattern == "var_trans") return ("log|exp|sqrt|inv\\(.+?\\)")

}
