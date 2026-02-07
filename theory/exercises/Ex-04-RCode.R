
library(ggplot2)
library(patchwork) # zum Anordnen von ggplot2 Objekten
theme_set(theme_bw() + theme(legend.position = "top"))


# a)

# relevanter Bereich (in dem die W'keit ungleich 0 ist)
untere_grenze <- 0
obere_grenze <- 30

# definiere Dichte und Verteilungsfunktion
density_fct <- function(x) dchisq(x, df = 10)
distribution_fct <- function(x) pchisq(x, df = 10)

# mit base plot
par(mfrow = c(1,2))
curve(density_fct, from = untere_grenze, to = obere_grenze,
      xlab = "x", ylab = "f(x)")
curve(distribution_fct, from = untere_grenze, to = obere_grenze,
      xlab = "x", ylab = "F(x)")
par(mfrow = c(1,1))

# mit ggplot2
dens_plot_gg <- ggplot(data.frame(x = c(untere_grenze, obere_grenze)),
                       aes(x = x)) +
  stat_function(fun = density_fct) +
  labs(x = "x", y = "f(x)")
dist_plot_gg <- ggplot(data.frame(x = c(untere_grenze, obere_grenze)),
                       aes(x = x)) +
  stat_function(fun = distribution_fct) +
  labs(x = "x", y = "F(x)")
# Plots anordnen
dens_plot_gg + dist_plot_gg


# b)

# Modus bestimmen: enweder grafisch, durch Optimieren oder Nachschauen
## nach Wikipedia: Modus von X ist max(df - 2, 0) = 8
modus <- 8

# Median bestimmen: enweder grafisch, durch bestimmen von F^-1(0.5) oder
# Nachschauen
median <- qchisq(0.5, df = 10) # ~9.34

# Erwartungswert bestimmen: nachschauen oder ausrechnen
## nach Wikipedia: E(X) = df
e_wert <- 10

# Verteilungsparameter speichern
vert_parameter <- data.frame(
  "Lageparameter" = factor(c("Modus", "Median", "Erwartungswert"),
                           levels = c("Modus", "Median", "Erwartungswert")),
  "Wert" = c(modus, median, e_wert),
  "Farbe" = c("coral", "skyblue", "gold2")
)

# Plot Dichte
dens_plot_gg <- ggplot(data.frame(x = c(untere_grenze, obere_grenze)),
                       aes(x = x)) +
  stat_function(fun = density_fct) +
  labs(x = "x", y = "f(x)") +
  geom_vline(data = vert_parameter,
             aes(xintercept = Wert, color = Lageparameter)) +
  scale_color_manual(values = vert_parameter$Farbe)
# Plot Verteilungsfunktion
dist_plot_gg <- ggplot(data.frame(x = c(untere_grenze, obere_grenze)),
                       aes(x = x)) +
  stat_function(fun = distribution_fct) +
  labs(x = "x", y = "F(x)") +
  geom_vline(data = vert_parameter,
             aes(xintercept = Wert, color = Lageparameter)) +
  geom_hline(yintercept = 0.5, linetype = "dashed",
             color = vert_parameter[vert_parameter$Lageparameter == "Median", "Farbe"]) +
  scale_color_manual(values = vert_parameter$Farbe)
# Plots anordnen
dens_plot_gg + dist_plot_gg +
  plot_annotation(theme = theme(legend.position = "top")) +
  plot_layout(guides = "collect")

# Erwartungswert > Median > Modus, wie bei einer rechtsschiefen Verteilung ueblich.


# c)
# i) Die Gewichte addieren sich zu 1.
# ii) Ja, das folgt durch die Linearität des Integrals.
# iii) 
# E(V) = 0.2 * E(X) + 0.3 * E(Y) + 0.5 * E(Z)
#      = 0.2 * -5 + 0.3 * 3 + 0.5 * 0
#      = -0.1


# d) und e)

untere_grenze <- -10
obere_grenze <- -untere_grenze

# definiere Dichte und Verteilungsfunktion
density_fct <- function(x) {
  0.2 * dnorm(x, mean = -5, sd = 1) +
    0.3 * dnorm(x, mean = 3, sd = sqrt(5)) +
    0.5 * dt(x, df = 10)
}
distribution_fct <- function(x) {
  0.2 * pnorm(x, mean = -5, sd = 1) +
    0.3 * pnorm(x, mean = 3, sd = sqrt(5)) +
    0.5 * pt(x, df = 10)
}


# Modus bestimmen
## keine bekannte Verteilung, daher mit Optimierung
modus <- optimize(density_fct, interval = c(-10, 10), maximum=TRUE)[[1]]

# Median bestimmen
## inverse Verteilfunktion nicht verfuegbar, daher Funktion invertieren um
## punktweise Werte berechnen zu koennen
inverse <- function (f, lower = -100, upper = 100) {
  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[[1]]
}
F_inverse <- inverse(distribution_fct, -10, 10)
# Median
median <- F_inverse(0.5)


# Erwartungswert bestimmen
## ueber Summe der einzelnen Zufallsvariablen
e_wert <- 0.2 * -5 + 0.3 * 3 + 0.5 * 0

# Verteilungsparameter speichern
vert_parameter <- data.frame(
  "Lageparameter" = factor(c("Modus", "Median", "Erwartungswert"),
                           levels = c("Modus", "Median", "Erwartungswert")),
  "Wert" = c(modus, median, e_wert),
  "Farbe" = c("coral", "skyblue", "gold2")
)

# Plot Dichte
dens_plot_gg <- ggplot(data.frame(x = c(untere_grenze, obere_grenze)),
                       aes(x = x)) +
  stat_function(fun = density_fct) +
  labs(x = "x", y = "f(x)") +
  geom_vline(data = vert_parameter,
             aes(xintercept = Wert, color = Lageparameter)) +
  scale_color_manual(values = vert_parameter$Farbe)
# Plot Verteilungsfunktion
dist_plot_gg <- ggplot(data.frame(x = c(untere_grenze, obere_grenze)),
                       aes(x = x)) +
  stat_function(fun = distribution_fct) +
  labs(x = "x", y = "F(x)") +
  geom_vline(data = vert_parameter,
             aes(xintercept = Wert, color = Lageparameter)) +
  geom_hline(yintercept = 0.5, linetype = "dashed",
             color = vert_parameter[vert_parameter$Lageparameter == "Median", "Farbe"]) +
  scale_color_manual(values = vert_parameter$Farbe)
# Plots anordnen
dens_plot_gg + dist_plot_gg +
  plot_annotation(theme = theme(legend.position = "top")) +
  plot_layout(guides = "collect")

# Die drei Verteilungsparameter liegen nah beieinander, die Verteilung ist aber
# nicht annaehernd symmetrisch.
