library(Hmisc)

mouse_doses <- c(0.012, 0.013, 0.015, 0.016, 0.017, 0.019, 0.022, 0.120, 0.125, 0.130, 0.170, 0.210, 0.250, 0.260, 0.270, 0.300, 0.320, 0.340, 0.350, 0.360, 0.370, 0.380, 0.400, 0.420, 0.500, 0.550, 0.600, 0.680, 0.700, 0.750, 0.900, 1.000)
mouse_response <- c(0.003940476, 0.004273810, 0.004952381, 0.005880952, 0.008190476, 0.008273810, 0.011523810, 0.070261905, 0.079863095, 0.081797619, 0.092595238, 0.114226190, 0.122345238, 0.126303571, 0.134761905, 0.138357143, 0.154803571, 0.160976190, 0.173535714, 0.168285714, 0.166833333, 0.172869048, 0.182500000, 0.194375000, 0.230785714, 0.234952381, 0.250791667, 0.279767857, 0.277488095, 0.291916667, 0.319916667, 0.332970238)
culture_doses <- c(0.038, 0.045, 0.050, 0.170, 0.200, 0.220, 0.270, 0.300, 0.340, 0.350, 0.360, 0.370, 0.380, 0.390, 0.400, 0.440, 0.450, 0.460, 0.500, 0.600, 0.680, 0.700, 0.800, 0.900, 1.000)
culture_response <- c(0.005119048, 0.006023810, 0.008464286, 0.051809524, 0.065309524, 0.071726190, 0.106851190, 0.140535714, 0.162523810, 0.177630952, 0.181666667, 0.182047619, 0.191488095, 0.207047619, 0.214642857, 0.240476190, 0.252059524, 0.268297619, 0.287845238, 0.372708333, 0.382678571, 0.384726190, 0.408797619, 0.405619048, 0.435285714)


plot(mouse_response ~ mouse_doses, pch=20, col="red", axes = F, main = "Mouse and Culture Dose Response Curve", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 0.50))
points(culture_response ~ culture_doses, pch=20, col="blue")

axis(1, col="dodgerblue", col.ticks="black", col.axis="Black", font = 2, cex.axis=1)
axis(2, col="dodgerblue", col.ticks="black", col.axis="black", font = 2, cex.axis=1)
mtext("Doses", side=1, line=3, col="red", cex=1, font=4)
mtext("Nectrig_Rate", side=2, line=3, col="red", cex=1, font = 4)

minor.tick(nx=5, ny=5, tick.ratio=0.5)
legend(0, 0.45, c("mouse", "culture"), c("red", "blue"), cex=1, text.font=2, lty = c(1, 1), bg='lightblue')
