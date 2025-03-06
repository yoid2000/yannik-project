# Multiply partially data synthesis of employee data of SES 2018

'Synthesis of Employee Dataset'
'm=5 synthetic datasets are generated and as many predictors are included as the function is able to consider'
'method: "": indicates tat variable is not synthesized but only considered as potential predictor by the algorithm'
'visit.sequence: initializes the order of synthesis (and predictor variables)'
syn_AN_PF <- syn(vse_AN_gwap_noNNA_PF, m=5, visit.sequence = c(2:16, 1, 18:22, 54, 23:37, 42, 88:89, 86:87, 40:41, 57, 38:39, 43, 44:51, 55:56, 58:85),
                 method = c("cart","","","","","","","","","","","","","","","","","","","","cart","cart","","","cart","","","","","","","","","","","","cart","cart","cart","~I(EF24_x * EF21)","~I(EF25_x * EF21)","cart","~I(12*EF21)","cart","","","","","","","","","~I(2018-EF12U2)","~I(2018-EF11)","","","~I(EF21-EF24-EF25)","","","","~I(EF21/(EF19+EF20))","","","","","","","","","cart","cart","","","","","","","","","","","","","","","~I(exp(EF24_z)/(1+exp(EF24_z)))","~I(exp(EF25_z)/(1+exp(EF25_z)))","normrank","normrank"), 
                 seed = 123, cart.minbucket = 3)


