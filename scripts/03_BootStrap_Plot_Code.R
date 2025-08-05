pdf("figures/bootstrap/boot_histograms.pdf", width=6, height=9)
hist(boot.volume, layout = c(5,4))
dev.off()

pdf("figures/bootstrap/sigma_histograms.pdf", width=6, height=9)
hist(boot.sigma.intensity, layout = c(5,4))
dev.off()

pdf("figures/bootstrap/red_histograms.pdf", width=6, height=9)
hist(boot.red.green.ratio, layout = c(5,4))
dev.off()

pdf("figures/bootstrap/aspect_histograms.pdf", width=6, height=9)
hist(boot.aspect.ratio, layout = c(5,4))
dev.off()
