#Custon R theme


library(ggplot2)

theme_minimal
#Want to add space between x and y axis title
theme_jack <- function () { 
  theme_minimal(base_size=12, base_family="Cairo") 
}

#    theme(plot.caption = element_text(lineheight = 0.5)

ggplot(data=health_party_long, aes(x=name, y=value, ymin=ci_min, ymax=ci_max, fill=pid_lean)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.5)+
  geom_errorbar(width = .05, position = position_dodge(0.5), color = "gray") +
  labs(title="",
       x="", y="Percentage\nwith\nCondition\n",
       caption = "Data from Cooperative Campaign Election Study 2012 (Self Reported)") +
  scale_y_continuous(limits = c(0, .35), breaks = seq(from = 0.0, to = .35, by = .05), 
                     labels = scales::percent_format(accuracy = 5L)) +
  theme_jack() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size=10), axis.text.x = element_text(size=10),
        legend.position = "none") +
  scale_fill_manual(values=c('#4d94ff','#cc3300'))  



