tab_mort_func <- function(base) {
  base %>% 
    group_by(year, edo, sex) %>% 
    mutate(mx = ifelse(pop > 0, deaths / pop, 1),
           ax = ifelse (
             age == 0, ifelse(
               sex == "Males", ifelse(
                 mx >= 0.107, 0.33, 0.045 + 2.684 * mx
               ), ifelse(
                 mx >= 0.107, 0.35, 0.053+2.8 * mx
               )
             ), ifelse( age == 109,
                        ifelse(mx==0, 0.5,
                               1 / mx), 0.5)),
           qx = ifelse(mx > 1, 1, mx / (1 +(1 - ax) * mx
           )),
           px = pmax(0, 1 - qx),
           lx = c(1, cumprod(px))[-length(px)],
           dx = lx * qx, 
           Lx = lead(lx, default = 0) + ax * dx,
           Tx = rev(cumsum(rev(Lx))), 
           ex = Tx / lx) %>% 
    ungroup() %>% 
    replace_na(list(ex = 0))
  }
