### simulate_rdata.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May 18 2023 (09:14) 
## Version: 
## Last-Updated: May 20 2023 (17:51) 
##           By: Thomas Alexander Gerds
##     Update #: 7
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
simulate_rdata <- function(n){
    prob_baseline <- c("Male" = 0.5985464)
    cov_model <- list(c(`(Intercept)` = -0.5792, sexMale = -0.1276),
                      c(`(Intercept)` = -3.7051, sexMale = -0.018, hypertension_0 = 7.8756),
                      c(`(Intercept)` = -3.7455, sexMale = -0.0375, hypertension_0 = -0.8138, hypertension_1 = 8.6942),
                      c(`(Intercept)` = -3.7606, sexMale = -0.0439, hypertension_0 = -0.2525, hypertension_1 = 0.1137, hypertension_2 = 8.0262),
                      c(`(Intercept)` = -3.7138,sexMale = -0.1124,hypertension_0 = -0.8392,hypertension_1 = 0.1095,hypertension_2 = 0.3076,hypertension_3 = 8.1833),
                      c(`(Intercept)` = -3.7994,sexMale = -0.0417,hypertension_0 = -0.2861,hypertension_1 = -0.4597,hypertension_2 = 0.4671,hypertension_3 = -0.7046,hypertension_4 = 8.8839),
                      c(`(Intercept)` = -3.6779,sexMale = -0.1955,hypertension_0 = -0.6062,hypertension_1 = -0.2974,hypertension_2 = -0.2724,hypertension_3 = 0.7916,hypertension_4 = -0.1144,hypertension_5 = 8.3115),
                      c(`(Intercept)` = -3.7832,sexMale = 0.0015,hypertension_0 = -0.7132,hypertension_1 = -0.0059,hypertension_2 = -0.1356,hypertension_3 = 0.3529,hypertension_4 = -0.3753,hypertension_5 = 0.441,hypertension_6 = 8.2286),
                      c(`(Intercept)` = -3.7545,sexMale = -0.0495,hypertension_0 = -1.377,hypertension_1 = 0.4148,hypertension_2 = 0.2757,hypertension_3 = 0.2469,hypertension_4 = -0.6087,hypertension_5 = 0.0952,hypertension_6 = 0.0635,hypertension_7 = 8.5181),
                      c(`(Intercept)` = -3.6402,sexMale = -0.0538,hypertension_0 = -1.2246,hypertension_1 = 0.2797,hypertension_2 = 0.6551,hypertension_3 = -0.6981,hypertension_4 = 0.4682,hypertension_5 = 0.2006,hypertension_6 = -0.2912,hypertension_7 = 0.3562,hypertension_8 = 7.8243))
    prop_model <- list("A" = list(c(`(Intercept)` = -1.6585, sexMale = -0.001, hypertension_0 = 0.2096),
                                  c(`(Intercept)` = -3.6964, sexMale = 0.1754, hypertension_0 = 0.0457, 
                                    GS_0 = 4.1533),
                                  c(`(Intercept)` = -3.6737, sexMale = 0.1531, 
                                    hypertension_0 = 0.0726, hypertension_1 = -0.0539, GS_0 = 1.2897, 
                                    GS_1 = 3.6853),
                                  c(`(Intercept)` = -3.6248, sexMale = 0.1572, 
                                    hypertension_0 = 0.0147, hypertension_1 = 0.0603, hypertension_2 = -0.0964, 
                                    GS_0 = 0.5424, GS_1 = 1.0581, GS_2 = 4.0273),
                                  c(`(Intercept)` = -3.639, 
                                    sexMale = 0.1781, hypertension_0 = 0.1077, hypertension_1 = -0.0321, 
                                    hypertension_2 = 0.1938, hypertension_3 = -0.3187, GS_0 = 0.4178, 
                                    GS_1 = 0.3598, GS_2 = 1.2203, GS_3 = 3.9876),
                                  c(`(Intercept)` = -3.5366, 
                                    sexMale = 0.0974, hypertension_0 = 0.0959, hypertension_1 = -0.238, 
                                    hypertension_2 = 0.1596, hypertension_3 = 0.0391, hypertension_4 = -0.1747, 
                                    GS_0 = 0.2816, GS_1 = 0.2868, GS_2 = 0.446, GS_3 = 1.2201, GS_4 = 3.9526
                                    ),
                                  c(`(Intercept)` = -3.5322, sexMale = 0.1206, hypertension_0 = 0.3388, 
                                    hypertension_1 = -0.4185, hypertension_2 = 0.2218, hypertension_3 = -0.223, 
                                    hypertension_4 = 0.0503, hypertension_5 = -0.0453, GS_0 = 0.3814, 
                                    GS_1 = -0.0034, GS_2 = 0.3712, GS_3 = 0.3804, GS_4 = 1.2657, 
                                    GS_5 = 4.0402),
                                  c(`(Intercept)` = -3.4513, sexMale = 0.0832, 
                                    hypertension_0 = 0.114, hypertension_1 = 0.2146, hypertension_2 = -0.2763, 
                                    hypertension_3 = -0.1256, hypertension_4 = 0.1473, hypertension_5 = -0.4621, 
                                    hypertension_6 = 0.2315, GS_0 = 0.3135, GS_1 = 0.2519, GS_2 = 0.2075, 
                                    GS_3 = 0.0725, GS_4 = 0.5737, GS_5 = 1.0696, GS_6 = 4.0789), 
                                  c(`(Intercept)` = -3.4861, sexMale = 0.1326, hypertension_0 = -0.0416, 
                                    hypertension_1 = -0.2083, hypertension_2 = 0.5674, hypertension_3 = -0.4857, 
                                    hypertension_4 = 0.2774, hypertension_5 = -0.1649, hypertension_6 = -0.083, 
                                    hypertension_7 = -0.0454, GS_0 = 0.5263, GS_1 = -0.2714, 
                                    GS_2 = 0.0904, GS_3 = 0.155, GS_4 = 0.1105, GS_5 = 0.5879, 
                                    GS_6 = 1.1734, GS_7 = 4.0654),
                                  c(`(Intercept)` = -3.4892, 
                                    sexMale = 0.1166, hypertension_0 = 0.0548, hypertension_1 = 0.2712, 
                                    hypertension_2 = -0.2189, hypertension_3 = 0.2562, hypertension_4 = -0.1538, 
                                    hypertension_5 = 0.0448, hypertension_6 = -0.3947, hypertension_7 = 0.2278, 
                                    hypertension_8 = -0.2, GS_0 = 0.4269, GS_1 = -0.1105, GS_2 = 0.0313, 
                                    GS_3 = 0.1046, GS_4 = 0.4071, GS_5 = 0.117, GS_6 = 0.4551, 
                                    GS_7 = 1.2972, GS_8 = 4.0326)))
    censoring_model <- list(c(`(Intercept)` = 3.2082, sexMale = -0.024, hypertension_0 = 0.1656, 
                              GS_0 = -1.5697),
                            c(`(Intercept)` = 2.8807,
                              sexMale = -0.0038, 
                              hypertension_0 = -0.1026,
                              hypertension_1 = 0.3252,
                              GS_0 = -1.4171, 
                              GS_1 = -0.2189),
                            c(`(Intercept)` = 2.9503, sexMale = 0.037, hypertension_0 = 0.0012, 
                              hypertension_1 = -0.2621, hypertension_2 = 0.5082, GS_0 = -1.2128, 
                              GS_1 = -0.1447, GS_2 = -0.4077),
                            c(`(Intercept)` = 2.9248, sexMale = -0.0214, 
                              hypertension_0 = -0.0944, hypertension_1 = 0.0722, hypertension_2 = -0.1251, 
                              hypertension_3 = 0.3904, GS_0 = -1.1662, GS_1 = 0.2834, GS_2 = -0.2823, 
                              GS_3 = -0.4124),
                            c(`(Intercept)` = 2.9095, sexMale = 0.0501, 
                              hypertension_0 = -0.1011, hypertension_1 = -0.2973, hypertension_2 = 0.4493, 
                              hypertension_3 = -0.4645, hypertension_4 = 0.6461, GS_0 = -1.0929, 
                              GS_1 = 0.0668, GS_2 = 0.1638, GS_3 = -0.1642, GS_4 = -0.5894), 
                            c(`(Intercept)` = 2.7677, sexMale = -0.0698, hypertension_0 = -0.2435, 
                              hypertension_1 = 0.2206, hypertension_2 = -0.1496, hypertension_3 = 0.046, 
                              hypertension_4 = -0.1182, hypertension_5 = 0.4317, GS_0 = -0.9971, 
                              GS_1 = 0.0533, GS_2 = 0.2283, GS_3 = 0.1222, GS_4 = -0.3772, 
                              GS_5 = -0.5404),
                            c(`(Intercept)` = 2.8292, sexMale = 0.0491, 
                              hypertension_0 = -0.463, hypertension_1 = 0.1173, hypertension_2 = 0.0659, 
                              hypertension_3 = -0.1466, hypertension_4 = 0.3203, hypertension_5 = -0.0656, 
                              hypertension_6 = 0.3591, GS_0 = -0.7709, GS_1 = -0.1703, 
                              GS_2 = 0.1693, GS_3 = 0.1679, GS_4 = 0.1303, GS_5 = -0.3627, 
                              GS_6 = -0.785),
                            c(`(Intercept)` = 2.7013, sexMale = -0.0257, 
                              hypertension_0 = -0.2577, hypertension_1 = -0.1097, hypertension_2 = 0.0879, 
                              hypertension_3 = 0.154, hypertension_4 = -0.1887, hypertension_5 = 0.0657, 
                              hypertension_6 = -0.1029, hypertension_7 = 0.4988, GS_0 = -0.7491, 
                              GS_1 = 0.1985, GS_2 = -0.2326, GS_3 = -0.147, GS_4 = 0.0809, 
                              GS_5 = 0.0076, GS_6 = -0.2598, GS_7 = -0.6535),
                            c(`(Intercept)` = 2.8029, 
                              sexMale = 0.0689, hypertension_0 = -0.2033, hypertension_1 = -0.1069, 
                              hypertension_2 = 0.0575, hypertension_3 = 0.2056, hypertension_4 = 0.1453, 
                              hypertension_5 = -0.1331, hypertension_6 = -0.1677, hypertension_7 = 0.2178, 
                              hypertension_8 = 0.1274, GS_0 = -0.6848, GS_1 = -0.1239, 
                              GS_2 = 0.0689, GS_3 = -0.1748, GS_4 = 0.0528, GS_5 = 0.0435, 
                              GS_6 = -0.09, GS_7 = -0.5505, GS_8 = -0.3883),
                            c(`(Intercept)` = 2.6784, 
                              sexMale = -0.0177, hypertension_0 = 0.025, hypertension_1 = 0.1201, 
                              hypertension_2 = -0.2195, hypertension_3 = 0.0031, hypertension_4 = 0.0143, 
                              hypertension_5 = -0.319, hypertension_6 = 0.3018, hypertension_7 = 0.1518, 
                              hypertension_8 = -0.1835, hypertension_9 = 0.2695, GS_0 = -0.5815, 
                              GS_1 = 0.0112, GS_2 = -0.1479, GS_3 = -0.1019, GS_4 = -0.0604, 
                              GS_5 = -0.1335, GS_6 = 0.0597, GS_7 = -0.3211, GS_8 = -0.2628, 
                              GS_9 = -0.4314))
    comp.event_model <- list(c(`(Intercept)` = -4.647, sexMale = -0.1007, hypertension_0 = 0.777, 
                               GS_0 = -0.5265),
                             c(`(Intercept)` = -4.834,
                               sexMale = 0.0672,
                               hypertension_0 = -0.2976, 
                               hypertension_1 = 1.0228,
                               GS_0 = -0.3386,
                               GS_1 = -0.3174),
                             c(`(Intercept)` = -4.8388, 
                               sexMale = 0.0623, hypertension_0 = 0.3071, hypertension_1 = -0.6997, 
                               hypertension_2 = 1.1217, GS_0 = -0.1415, GS_1 = -0.2473, GS_2 = -0.2674
                               ),
                             c(`(Intercept)` = -4.9018, sexMale = 0.2457, hypertension_0 = -0.0868, 
                               hypertension_1 = 0.3722, hypertension_2 = -0.156, hypertension_3 = 0.6076, 
                               GS_0 = -0.2534, GS_1 = 0.0613, GS_2 = 0.065, GS_3 = -0.9289),
                             c(`(Intercept)` = -4.7314, sexMale = 0.1344, hypertension_0 = 0.1576, 
                               hypertension_1 = 0.0558, hypertension_2 = -0.2873, hypertension_3 = -0.146, 
                               hypertension_4 = 0.9141, GS_0 = -0.217, GS_1 = -0.0984, GS_2 = -0.0711, 
                               GS_3 = -0.0791, GS_4 = -0.4477),
                             c(`(Intercept)` = -4.8357, 
                               sexMale = 0.1414, hypertension_0 = 0.4151, hypertension_1 = -0.2238, 
                               hypertension_2 = -0.3694, hypertension_3 = 0.1221, hypertension_4 = -0.2722, 
                               hypertension_5 = 1.0572, GS_0 = -0.5494, GS_1 = 0.4525, GS_2 = 0.1574, 
                               GS_3 = 0.0826, GS_4 = -0.0204, GS_5 = -0.8605),
                             c(`(Intercept)` = -4.77, 
                               sexMale = -0.0157, hypertension_0 = 0.0664, hypertension_1 = -0.1458, 
                               hypertension_2 = 0.5466, hypertension_3 = -0.8251, hypertension_4 = 0.0307, 
                               hypertension_5 = 0.1281, hypertension_6 = 1.0102, GS_0 = -0.4538, 
                               GS_1 = 0.3621, GS_2 = -0.4869, GS_3 = 0.3247, GS_4 = -0.285, 
                               GS_5 = -0.2745, GS_6 = -0.47),
                             c(`(Intercept)` = -4.6684, 
                               sexMale = 0.1157, hypertension_0 = -0.1716, hypertension_1 = 0.3973, 
                               hypertension_2 = -0.2048, hypertension_3 = 0.2752, hypertension_4 = 0.2649, 
                               hypertension_5 = -0.5606, hypertension_6 = -0.6807, hypertension_7 = 1.3286, 
                               GS_0 = -0.0976, GS_1 = 0.1097, GS_2 = -0.4993, GS_3 = 0.0656, 
                               GS_4 = -0.0283, GS_5 = 0.3463, GS_6 = -0.0412, GS_7 = -1.2783
                               ),
                             c(`(Intercept)` = -4.6622, sexMale = 0.1237, hypertension_0 = 0.011, 
                               hypertension_1 = -0.0781, hypertension_2 = 0.432, hypertension_3 = 0.3017, 
                               hypertension_4 = -0.9993, hypertension_5 = 0.6972, hypertension_6 = -0.2647, 
                               hypertension_7 = -0.298, hypertension_8 = 0.9008, GS_0 = -0.1563, 
                               GS_1 = 0.343, GS_2 = 0.009, GS_3 = 0.4082, GS_4 = -0.6976, 
                               GS_5 = -0.0686, GS_6 = 0.2798, GS_7 = -0.5394, GS_8 = -0.9466
                               ))
    outcome_model <- list(c(`(Intercept)` = -4.9484, sexMale = 0.6052, hypertension_0 = 1.0752, 
                            GS_0 = 0.5426),
                          c(`(Intercept)` = -5.6013, sexMale = 0.5933, hypertension_0 = 0.0886, 
                            hypertension_1 = 0.9336, GS_0 = 0.3198, GS_1 = -0.4368),
                          c(`(Intercept)` = -5.7271, 
                            sexMale = 0.5439, hypertension_0 = 0.7007, hypertension_1 = -0.8709, 
                            hypertension_2 = 1.2628, GS_0 = 0.0627, GS_1 = 0.0987, GS_2 = -0.5464
                            ),
                          c(`(Intercept)` = -5.6481, sexMale = 0.2891, hypertension_0 = 0.9296, 
                            hypertension_1 = -0.6693, hypertension_2 = 0.2746, hypertension_3 = 0.6618, 
                            GS_0 = 0.0317, GS_1 = -0.5041, GS_2 = 0.356, GS_3 = -0.2025), 
                          c(`(Intercept)` = -5.6157, sexMale = 0.3287, hypertension_0 = 7e-04, 
                            hypertension_1 = 0.6375, hypertension_2 = -0.1554, hypertension_3 = -0.2237, 
                            hypertension_4 = 0.762, GS_0 = -0.1005, GS_1 = -0.3012, GS_2 = -0.044, 
                            GS_3 = 0.1488, GS_4 = -0.0478),
                          c(`(Intercept)` = -5.6855, 
                            sexMale = 0.5219, hypertension_0 = 0.3468, hypertension_1 = -0.0732, 
                            hypertension_2 = -0.1408, hypertension_3 = -0.1237, hypertension_4 = 0.3314, 
                            hypertension_5 = 0.534, GS_0 = 0.3179, GS_1 = -0.1579, GS_2 = 0.0863, 
                            GS_3 = 0.467, GS_4 = -0.0155, GS_5 = -0.749),
                          c(`(Intercept)` = -5.7747, 
                            sexMale = 0.514, hypertension_0 = 0.5962, hypertension_1 = 0.0647, 
                            hypertension_2 = -0.4627, hypertension_3 = 0.8488, hypertension_4 = -0.6135, 
                            hypertension_5 = -0.6194, hypertension_6 = 1.1247, GS_0 = -0.3115, 
                            GS_1 = 0.5535, GS_2 = 0.1949, GS_3 = 0.0562, GS_4 = -0.5318, 
                            GS_5 = 0.3374, GS_6 = -0.5651),
                          c(`(Intercept)` = -5.7744, 
                            sexMale = 0.319, hypertension_0 = 0.4314, hypertension_1 = -0.0907, 
                            hypertension_2 = 0.4745, hypertension_3 = -0.9469, hypertension_4 = 1.3166, 
                            hypertension_5 = -1.1149, hypertension_6 = 1.1008, hypertension_7 = -0.1467, 
                            GS_0 = -0.3144, GS_1 = 0.2431, GS_2 = 0.6514, GS_3 = 0.7688, 
                            GS_4 = -1.0972, GS_5 = 0.4256, GS_6 = -0.1057, GS_7 = -0.5344
                            ),
                          c(`(Intercept)` = -5.8001, sexMale = 0.5111, hypertension_0 = 0.823, 
                            hypertension_1 = -0.0782, hypertension_2 = 0.3343, hypertension_3 = -0.7539, 
                            hypertension_4 = 0.1657, hypertension_5 = -0.4698, hypertension_6 = 0.471, 
                            hypertension_7 = -0.0737, hypertension_8 = 0.7066, GS_0 = -0.2083, 
                            GS_1 = -0.1091, GS_2 = -0.05, GS_3 = -0.237, GS_4 = 0.582, 
                            GS_5 = -0.1843, GS_6 = -0.1668, GS_7 = 0.2782, GS_8 = -0.4284
                            ),
                          c(`(Intercept)` = -5.876, sexMale = 0.702, hypertension_0 = 0.53, 
                            hypertension_1 = 0.0749, hypertension_2 = -0.0107, hypertension_3 = -0.2332, 
                            hypertension_4 = 0.391, hypertension_5 = 0.1572, hypertension_6 = -0.1617, 
                            hypertension_7 = -0.5171, hypertension_8 = -0.2612, hypertension_9 = 1.0251, 
                            GS_0 = 0.1882, GS_1 = 0.0713, GS_2 = -0.5032, GS_3 = -0.6027, 
                            GS_4 = -0.3316, GS_5 = -0.2897, GS_6 = -0.0469, GS_7 = 0.119, 
                            GS_8 = -0.9366, GS_9 = 0.2279))
    m <- get_lava_model(time_horizon = 10,
                        name_baseline_covariates = "V",
                        name_time_covariates = "L", 
                        name_regimen = "A",
                        name_outcome = "Y",
                        name_censoring = "C",
                        name_comp.event = "D", 
                        censoring_model = censoring_model,
                        comp.event_model = comp.event_model,
                        outcome_model = outcome_model,
                        prop_model = prop_model,
                        prob_baseline = prob_baseline,
                        cov_model = cov_model)
    d <- get_sim_data(m, n)
    d[]
}


######################################################################
### simulate_rdata.R ends here
