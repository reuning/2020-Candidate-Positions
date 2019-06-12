# setwd("/Users/kevinreuning/Dropbox/Projects/DFP/Candidate_Position/")
rm(list=ls())
df <- read.csv("dem_pos.csv", strip.white = T)

all_cands <- c("Bernie Sanders", "John Delaney",
               "Michael Bennet",  "Joe Biden", "Cory Booker", "Steve Bullock",
               "Pete Buttigieg", "Julian Castro",   "Bill DeBlasio",
               "Tulsi Gabbard",   "Kirsten Gillibrand", "Kamala Harris",   "John Hickenlooper",
               "Jay Inslee", "Amy Klobuchar",   "Wayne Messam", "Seth Moulton", "Beto O'Rourke",
               "Tim Ryan", "Eric Swalwell",   "Elizabeth Warren",
               "Marianne Williamson", "Andrew Yang")

que_cat <- apply(df, 2, function(x) length(unique(na.omit(x))))

Y_2 <- df[,que_cat==2]
for(ii in 1:ncol(Y_2)){
  if(0 %in% Y_2[,ii]) Y_2[,ii] <- Y_2[,ii] + 1
}


rownames(Y_2) <- df$Candidate
Y_2 <- reshape::melt(as.matrix(Y_2))
Y_2 <- Y_2[!is.na(Y_2$value),]
iss_2 <- as.numeric(factor(Y_2$X2, levels=names(que_cat)[que_cat==2]))
cand_2 <- as.numeric(factor(Y_2$X1, levels=all_cands))
Y_2 <- Y_2$value
N_2 <- length(Y_2)
K_2 <- max(iss_2)


Y_3 <- df[,que_cat==3]
for(ii in 1:ncol(Y_3)){
  if(0 %in% Y_3[,ii]) Y_3[,ii] <- Y_3[,ii] + 1
}


rownames(Y_3) <- df$Candidate
Y_3 <- reshape::melt(as.matrix(Y_3))
Y_3 <- Y_3[!is.na(Y_3$value),]
iss_3 <- as.numeric(factor(Y_3$X2, levels=names(que_cat)[que_cat==3]))
cand_3 <- as.numeric(factor(Y_3$X1, levels=all_cands))
Y_3 <- Y_3$value
N_3 <- length(Y_3)
K_3 <- max(iss_3)

library(rstan)

issue_dic <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQFVPycLqEw4ytS3_3N-IJg4bbfy9NdwEu8PUnbDG75V0FpMh-4B5ZZa1mUKxrg02iHfwDWjD1OAMyJ/pub?gid=0&single=true&output=csv", 
                      stringsAsFactors = F, strip.white = T)

issue_dic$Cats <- table(issue_dic$Issue)[issue_dic$Issue]

survey <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSqOMYA7xbzZlt2mH6Tdb4-0WY2eYJdnS6QlTHa4ng3HBNib7HwpzaHFIGeopCkTtYqcTZtAZ0iAYh4/pub?gid=1258402347&single=true&output=csv", 
                   stringsAsFactors = F, na.strings="", strip.white = T)
survey <- survey[,-1]

nn <- nrow(survey)
survey_trans <- matrix(NA, nrow=prod(dim(survey)),
                       ncol=9)
jj <- 1
for(ii in 1:ncol(survey)){
  survey_trans[((ii-1)*nn+1):(nn*ii),1] <- ii
  survey_trans[((ii-1)*nn+1):(nn*ii),2] <- 1:nn
  survey_trans[((ii-1)*nn+1):(nn*ii),3] <- issue_dic$Coding[match(survey[,ii], issue_dic$Position)] 
  survey_trans[((ii-1)*nn+1):(nn*ii),4] <- issue_dic$Cats[match(survey[,ii], issue_dic$Position)] 
  survey_trans[((ii-1)*nn+1):(nn*ii),5] <- issue_dic$Issue[match(survey[,ii], issue_dic$Position)] 
}


key_1 <- by(survey_trans[,5], survey_trans[,1], function(x) unique(na.omit(x)))
key_2 <- by(survey_trans[,3], survey_trans[,1], function(x) unique(na.omit(x)))

for(ii in 1:nrow(survey_trans)){
  tmp <- unlist(key_1[survey_trans[ii,1]])
  if(length(tmp)==2) {
    survey_trans[ii,6] <- as.character(tmp[1])
    survey_trans[ii,7] <- as.character(tmp[2])
  } else {
    survey_trans[ii,6] <- as.character(tmp[1])
    
  }
  
  
  tmp <- unlist(key_2[survey_trans[ii,1]])
  if(length(tmp)==2) {
    survey_trans[ii,8] <- as.character(tmp[1])
    survey_trans[ii,9] <- as.character(tmp[2])
  } else {
    survey_trans[ii,8] <- as.character(tmp[1])
    
  }
}
for(ii in 1:nrow(survey_trans)){
  if(is.na(survey_trans[ii,9]) & !is.na(survey_trans[ii,7]))
    survey_trans[ii,9] <- survey_trans[ii,8]
}
# survey_trans <- survey_trans[,-5]
colnames(survey_trans) <- c("question", "resp_id", "iss_choice", "cats",  "cat_choice", "iss_cat_1", "iss_cat_2", "iss_ch_1", 
                            "iss_ch_2")

survey_trans <- as.data.frame(survey_trans, stringsAsFactors=F)

cat <- c("6"="paris", "25"="accept_refugee", "27"="ban_ff_export", "28"="private_ins", 
         "29"="private_ins", "30"="elim_ec", "31"="elim_ec", "32"="ice_abolish", "33"="ice_abolish",
         "34"="paris", "35"="border_funding")
cho <- c("6"="3", "25"="2", "27"="1", "28"="1","29"="2","30"="1", "31"="2",
         "32"="1", "33"="2", "34"="2", "35"="1")

survey_trans$iss_cat_2[is.na(survey_trans$iss_cat_2)] <- cat[survey_trans$question[is.na(survey_trans$iss_cat_2)]]
survey_trans$iss_ch_2[is.na(survey_trans$iss_ch_2)] <- cho[survey_trans$question[is.na(survey_trans$iss_ch_2)]]
survey_trans <- survey_trans[!is.na(survey_trans$iss_choice),]
survey_cat_2 <- survey_trans[que_cat[survey_trans$iss_cat_1] ==2 &
                               que_cat[survey_trans$iss_cat_2]  ==2,]

iss_cat_2_1 <- survey_cat_2$iss_cat_1
iss_cat_2_2 <- survey_cat_2$iss_cat_2

iss_cat_2_1 <- as.numeric(factor(iss_cat_2_1, levels=names(que_cat)[que_cat==2]))
iss_cat_2_2 <- as.numeric(factor(iss_cat_2_2, levels=names(que_cat)[que_cat==2]))

iss_ch_2_1 <- as.numeric(survey_cat_2$iss_ch_1) 
iss_ch_2_2 <- as.numeric(survey_cat_2$iss_ch_2) 

resp_2 <- as.numeric(survey_cat_2$resp_id)
ch_2 <- (survey_cat_2$iss_choice == survey_cat_2$iss_ch_1) &
  (survey_cat_2$cat_choice == survey_cat_2$iss_cat_1)
ch_2 <- 1*ch_2


survey_cat_3 <- survey_trans[que_cat[survey_trans$iss_cat_1] ==3 &
                               que_cat[survey_trans$iss_cat_2]  ==3,]

iss_cat_3_1 <- survey_cat_3$iss_cat_1
iss_cat_3_2 <- survey_cat_3$iss_cat_2

iss_cat_3_1 <- as.numeric(factor(iss_cat_3_1, levels=names(que_cat)[que_cat==3]))
iss_cat_3_2 <- as.numeric(factor(iss_cat_3_2, levels=names(que_cat)[que_cat==3]))

iss_ch_3_1 <- as.numeric(survey_cat_3$iss_ch_1)
iss_ch_3_2 <- as.numeric(survey_cat_3$iss_ch_2)

resp_3 <- as.numeric(survey_cat_3$resp_id)
ch_3 <- (survey_cat_3$iss_choice == survey_cat_3$iss_ch_1) &
  (survey_cat_3$cat_choice == survey_cat_3$iss_cat_1)
ch_3 <- 1*ch_3



survey_mix <- survey_trans[!(que_cat[survey_trans$iss_cat_1] ==3 &
                               que_cat[survey_trans$iss_cat_2]  ==3) &
                               !(que_cat[survey_trans$iss_cat_1] ==2 &
                                   que_cat[survey_trans$iss_cat_2]  ==2) ,]
que_cat[survey_mix$iss_cat_1]

iss_cat_mix_1 <- survey_mix$iss_cat_1
iss_cat_mix_2 <- survey_mix$iss_cat_2

iss_cat_mix_1[que_cat[survey_mix$iss_cat_1]==2] <- survey_mix$iss_cat_2[que_cat[survey_mix$iss_cat_1]==2]
iss_cat_mix_2[que_cat[survey_mix$iss_cat_2]==3] <- survey_mix$iss_cat_1[que_cat[survey_mix$iss_cat_2]==3]

iss_cat_mix_1 <- as.numeric(factor(iss_cat_mix_1, levels=names(que_cat)[que_cat==3]))
iss_cat_mix_2 <- as.numeric(factor(iss_cat_mix_2, levels=names(que_cat)[que_cat==2]))

iss_ch_mix_1 <- as.numeric(survey_mix$iss_ch_1) 
iss_ch_mix_2 <- as.numeric(survey_mix$iss_ch_2)

iss_ch_mix_1[que_cat[survey_mix$iss_cat_1]==2] <- survey_mix$iss_ch_2[que_cat[survey_mix$iss_cat_1]==2]
iss_ch_mix_2[que_cat[survey_mix$iss_cat_2]==3] <- survey_mix$iss_ch_1[que_cat[survey_mix$iss_cat_2]==3]

iss_ch_mix_1 <- as.numeric(iss_ch_mix_1) 
iss_ch_mix_2 <- as.numeric(iss_ch_mix_2) 


resp_mix <- as.numeric(survey_mix$resp_id)
ch_mix <- (que_cat[survey_mix$cat_choice] == 3)
ch_mix <- 1*ch_mix

N_resp_2 <-length(ch_2)
N_resp_3 <-length(ch_3)
N_resp_mix <- length(ch_mix)
J <- max(c(resp_3, resp_2, resp_mix))


N <- length(all_cands)

mod <- stan(file="model.stan", cores=2, chains=4, control=list("adapt_delta"=.9))

# source("/Users/kevinreuning/Dropbox/Projects/DFP_Sec/DFP_Style.R")
# setwd("/Users/kevinreuning/Dropbox/Projects/DFP/Candidate_Position/")

tmp <- stan_plot(mod, "theta_cand") 
plot_df <- tmp$data
plot_df$params <- all_cands
png("cand_pos.png", height=8, width=6, units="in", res=200)
ggplot(plot_df, aes(x=reorder(params, m), y=m, ymin=ll, ymax=hh)) + 
  geom_pointrange() + coord_flip() + 
  # theme_dfp() + 
  theme(plot.caption = element_text(hjust=0, size=6)) + 
  labs(x="", y="Progressivism", caption="Scores estimated using issue positions collected by WaPo.\nEstimates made using an LVM and elite survey of issue progressivism.\nhttps://www.dataforprogress.org/blog/2019/6/11/who-is-the-most-progressive-democrat") +
  # ggtitle_dfp("Who is the most\nprogressive Democratic candidate?") 
  NULL
dev.off()


issue_dic_2 <- issue_dic[issue_dic$Cats==2,]
issue_dic_2$Issue <- factor(issue_dic_2$Issue, levels=names(que_cat)[que_cat==2])
issue_dic_2 <- issue_dic_2[order(issue_dic_2$Coding),]
issue_dic_2 <- issue_dic_2[order(issue_dic_2$Issue),]
issue_dic_2$Position[25] <- "Opposes increased border security funding, including resources to process asylum seekers"
issue_dic_2$Position[26] <- "Supports increased border security funding, including resources to process asylum seekers"

# stan_trace(mod, "theta_cand")
# stan_trace(mod, "theta_iss_2")
tmp <- stan_plot(mod, "theta_iss_2") 
plot_df <- tmp$data
plot_df$params <- factor(issue_dic_2$Position, levels=issue_dic_2$Position)

png("iss_pos_2.png", height=10, width=8, units="in", res=200)

ggplot(plot_df, aes(x=params, y=m, ymin=ll, ymax=hh)) + 
  geom_pointrange() + coord_flip() + 
  # theme_dfp() + 
  theme(plot.caption = element_text(hjust=0, size=6), 
        panel.grid.major = element_line(linetype='dashed', color="gray")) + 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 50)) +
  labs(x="", y="Progressivism", caption="Scores estimated using issue positions collected by WaPo.\nEstimates are the mean number of progressive positions held.\nhttps://www.dataforprogress.org/blog/2019/6/11/who-is-the-most-progressive-democrat") +
  # ggtitle_dfp("Progressivism of Issues\nwith 2 Categories")
  NULL
dev.off()
  


issue_dic_3 <- issue_dic[issue_dic$Cats==3,]
issue_dic_3$Issue <- factor(issue_dic_3$Issue, levels=names(que_cat)[que_cat==3])
issue_dic_3 <- issue_dic_3[order(issue_dic_3$Coding),]
issue_dic_3 <- issue_dic_3[order(issue_dic_3$Issue),]


issue_dic_3$Position[14] <- "Would cover undocumented immigrants in a health plan only under certain circumstances"

issue_dic_3$Position[32] <- "Would repeal penalities for those apprehended crossing border if part of a comprehensive plan"

issue_dic_3$Position[34] <- "Supports requiring E-Verify if it is improved"
issue_dic_3$Position[35] <- "Supports requiring E-Verify as part of a comprehensive immigration plan"

tmp <- stan_plot(mod, "theta_iss_3") 
plot_df <- tmp$data
plot_df$params <- factor(issue_dic_3$Position, levels=issue_dic_3$Position)
png("iss_pos_3.png", height=10, width=8, units="in", res=200)

ggplot(plot_df, aes(x=params, y=m, ymin=ll, ymax=hh)) + 
  geom_pointrange() + coord_flip() + 
  # theme_dfp() + 
  theme(plot.caption = element_text(hjust=0, size=6), 
        panel.grid.major = element_line(linetype='dashed', color="gray")) + 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 50)) +
  labs(x="", y="Progressivism", caption="Scores estimated using issue positions collected by WaPo.\nEstimates are the mean number of progressive positions held.\nhttps://www.dataforprogress.org/blog/2019/6/11/who-is-the-most-progressive-democrat") +
  # ggtitle_dfp("Progressivism of Issues\nwith 3 Categories")
  NULL
dev.off()

names(que_cat)[que_cat==2]
stan_trace(mod, "theta_iss_3")

names(que_cat)[que_cat==3]

stan_plot(mod, "beta_iss_3")
stan_plot(mod, "beta_iss_2")
stan_plot(mod, "beta_resp")




df_add <- df
for(ii in 2:ncol(df_add)){
  df_add[,ii] <- as.numeric(as.factor(df_add[,ii])) - 1
  if(2 %in% df_add[,ii]) df_add[,ii] <- df_add[,ii]/2
}

tmp <- as.data.frame(apply(df_add[,-1], 1, mean, na.rm=T))
tmp$Candidate <- df_add$Candidate
names(tmp)[1] <- "MeanScale"


plot_df <- stan_plot(mod, "theta_cand") 
plot_df <- plot_df$data
plot_df$params <- all_cands

plot_df <- merge(tmp, plot_df, by.x="Candidate", by.y="params")


png("cand_naieve.png", height=8, width=6, units="in", res=200)
ggplot(tmp, aes(x=reorder(Candidate, MeanScale), y=MeanScale)) + 
  geom_point() + coord_flip() + 
  # theme_dfp() + 
  theme(plot.caption = element_text(hjust=0, size=6)) + 
  labs(x="", y="Naieve Progressivism", 
       caption="Scores estimated using issue positions collected by WaPo.\nEstimates are the mean number of progressive positions held.\nSee for more details.") +
  # ggtitle_dfp("Naieve Estimates of Candidate Positions") 
  NULL
dev.off()

library(ggrepel)
png("cand_naieve.png", height=8, width=6, units="in", res=200)
ggplot(plot_df, aes(x=MeanScale, y=m, label=Candidate)) + geom_text_repel() +
  theme_dfp() + theme(panel.grid.major = element_blank()) +
  ggtitle_dfp("Latent vs Mean\nIdeology Estimates") + 
  labs(x="Mean Estimate", y="Latent Estimate")
dev.off()
