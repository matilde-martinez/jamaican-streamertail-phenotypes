#EEB RESEARCH DAY PRESENTATION FIGURES


dorsal.red.prop.RECOLORIZE <- read.csv("C:/Users/matil/OneDrive/Desktop/dorsal recolorize/updated.dorsal.red.prop.df.csv")
ventral.red.prop.RECOLORIZE <- read.csv("C:/Users/matil/OneDrive/Desktop/ventral recolorize/updated.four.color.ventral.red.prop.df.csv")
comb_meas<-read.csv("C:/Users/matil/OneDrive/Desktop/Aguillon Lab/Streamertail Bill Measurements - COMBINED.csv")

dorsal.pss.df <- as.data.frame(cbind(dorsal.red.prop.RECOLORIZE$samples..AS.THEY.APPEAR.IN.MASTER.GOOGLE.SHEETS., dorsal.red.prop.RECOLORIZE$red.prop, comb_meas$MMN_score, comb_meas$Sex))
ventral.pss.df <- as.data.frame(cbind(ventral.red.prop.RECOLORIZE$samples..AS.THEY.APPEAR.IN.MASTER.GOOGLE.SHEETS., ventral.red.prop.RECOLORIZE$red.prop, comb_meas$MMN_score, comb_meas$Sex))

dorsal.pss.df <- dorsal.pss.df[1:204, ]
ventral.pss.df <- ventral.pss.df[1:204, ]

colnames(dorsal.pss.df) <- c("ID NUMBER", "PROP.RED", "SCORE", "SEX")
colnames(ventral.pss.df) <- c("ID NUMBER", "PROP.RED", "SCORE", "SEX")

dorsal.pss.df$PROP.RED <- as.numeric(dorsal.pss.df$PROP.RED)
ventral.pss.df$PROP.RED <- as.numeric(ventral.pss.df$PROP.RED)

dorsal.pss.df <- dorsal.pss.df[!is.na(dorsal.pss.df$PROP.RED), ]
dorsal.pss.df <- dorsal.pss.df[!is.na(dorsal.pss.df$SCORE), ]
dorsal.pss.df <- dorsal.pss.df[!is.na(dorsal.pss.df$SEX), ]
ventral.pss.df <- ventral.pss.df[!is.na(ventral.pss.df$PROP.RED), ]
ventral.pss.df <- ventral.pss.df[!is.na(ventral.pss.df$SCORE), ]
ventral.pss.df <- ventral.pss.df[!is.na(ventral.pss.df$SEX), ]

female_dorsal_measurements_RECOLORIZE <- dorsal.pss.df[dorsal.pss.df$SEX == "2",]
female_ventral_measurements_RECOLORIZE <- ventral.pss.df[ventral.pss.df$SEX == "2",]
male_dorsal_measurements_RECOLORIZE <- dorsal.pss.df[dorsal.pss.df$SEX == "1",]
male_ventral_measurements_RECOLORIZE <- ventral.pss.df[ventral.pss.df$SEX == "1",]




# male dorsal and ventral plots

#VERSION 1: B&W

ggplot(male_dorsal_measurements_RECOLORIZE, aes(x = factor(SCORE), y = PROP.RED)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Proportion of Red in Male Dorsal Bills",
       x = "Scores") +
  geom_jitter(position = position_jitter(0.2))

ggplot(male_ventral_measurements_RECOLORIZE, aes(x = factor(SCORE), y = PROP.RED)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Proportion of Red in Male Ventral Bills",
       x = "Scores") +
  geom_jitter(position = position_jitter(0.2))



#VERSION 2: COLORS

library(ggplot2)

ggplot(male_dorsal_measurements_RECOLORIZE, aes(x = factor(SCORE), y = PROP.RED, fill = SCORE)) +
  geom_boxplot() +
  scale_fill_manual(values = c("black", "lightcoral", "lightcoral", "firebrick3","firebrick3")) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Proportion of Red in Male Dorsal Bills",
       x = "Scores") +
  geom_jitter(position = position_jitter(0.2))

ggplot(male_ventral_measurements_RECOLORIZE, aes(x = factor(SCORE), y = PROP.RED, fill = SCORE)) +
  geom_boxplot() +
  scale_fill_manual(values = c("black", "lightcoral", "lightcoral", "firebrick3","firebrick3")) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Proportion of Red in Male Ventral Bills",
       x = "Scores") +
  geom_jitter(position = position_jitter(0.2))



#VERSION 3: VIOLIN PLOT B&W
ggplot(male_dorsal_measurements_RECOLORIZE, aes(x = factor(SCORE), y = PROP.RED)) +
  geom_violin() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Proportion of Red in Male Dorsal Bills",
       x = "Scores") +
  geom_jitter(position = position_jitter(0.2))

ggplot(male_ventral_measurements_RECOLORIZE, aes(x = factor(SCORE), y = PROP.RED)) +
  geom_violin() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Proportion of Red in Male Ventral Bills",
       x = "Scores") +
  geom_jitter(position = position_jitter(0.2))



#VERSION 4: VIOLIN PLOT COLORS

ggplot(male_dorsal_measurements_RECOLORIZE, aes(x = factor(SCORE), y = PROP.RED, fill = SCORE)) +
  geom_violin() +
  scale_fill_manual(values = c("black", "lightcoral", "lightcoral", "firebrick3","firebrick3")) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Proportion of Red in Male Dorsal Bills",
       x = "Scores") +
  geom_jitter(position = position_jitter(0.2))

ggplot(male_ventral_measurements_RECOLORIZE, aes(x = factor(SCORE), y = PROP.RED, fill = SCORE)) +
  geom_violin() +
  scale_fill_manual(values = c("black", "lightcoral", "lightcoral", "firebrick3","firebrick3")) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Proportion of Red in Male Ventral Bills",
       x = "Scores") +
  geom_jitter(position = position_jitter(0.2))



# t-test comparing male means


male_dorsal_1 <- male_dorsal_measurements_RECOLORIZE$PROP.RED[male_dorsal_measurements_RECOLORIZE$SCORE == 1]
male_dorsal_2 <- male_dorsal_measurements_RECOLORIZE$PROP.RED[male_dorsal_measurements_RECOLORIZE$SCORE == 2]

male_dorsal_3 <- male_dorsal_measurements_RECOLORIZE$PROP.RED[male_dorsal_measurements_RECOLORIZE$SCORE == 3]
male_dorsal_4 <- male_dorsal_measurements_RECOLORIZE$PROP.RED[male_dorsal_measurements_RECOLORIZE$SCORE == 4]


male_ventral_3 <- male_ventral_measurements_RECOLORIZE$PROP.RED[male_ventral_measurements_RECOLORIZE$SCORE == 3]
male_ventral_4 <- male_ventral_measurements_RECOLORIZE$PROP.RED[male_ventral_measurements_RECOLORIZE$SCORE == 4]

male_dorsal_measurements_RECOLORIZE$SCORE <- as.numeric(male_dorsal_measurements_RECOLORIZE$SCORE)
male_ventral_measurements_RECOLORIZE$SCORE <- as.numeric(male_ventral_measurements_RECOLORIZE$SCORE)



t.test(male_dorsal_1, male_dorsal_2, conf.level = 0.99)

t.test(male_dorsal_3, male_dorsal_4, conf.level = 0.99)

t.test(male_ventral_3, male_ventral_4, conf.level = 0.99)




