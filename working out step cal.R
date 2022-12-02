## testing out lag function


lag(1:5)

lead(1:5)
x <- 1:5

test_df <- data.frame(x = c(1782673.369,
                            1782682.997,
                            1782680.561,
                            1782682.52,
                            1782681.667,
                            1782684.29,
                            1782684.925,
                            1782669.492,
                            1782678.751,
                            1782679.039),
                      y = c(2402296.712,
                            2402292.331,
                            2402296.647,
                            2402297.628,
                            2402290.179,
                            2402297.748,
                            2402273.597,
                            2402309.231,
                            2402299.204,
                            2402304.452
                      ))




test_df_1 <- test_df %>% 
  dplyr::mutate((step_x = (lag(x) - x)^ 2),
                (step_y = (lag(y) - y)^ 2),
                step_check = sqrt(step_x + step_y),
                step = sqrt( ((lag(x) - x)^ 2) + ((lag(y) - y)^ 2) )
                )
                
                
test_df_1
test_df_2 <- test_df %>% 
  dplyr::mutate(step = sqrt( ((lead(x) - x)^ 2) + ((lead(y) - y)^ 2) ) )

test_df_2
# the number match my example but its is the wrong row....


