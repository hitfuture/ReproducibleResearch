legs_df <- route( from = "marrs science, baylor university",
                  to="220 south 3rd street, waco, tx 76701", 
                  alternatives = TRUE)
qmap("424 clay avenue, waco, tx", zoom = 15, maptype = "hybrid",
     base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
        geom_leg(
                aes(x = startLon, y = startLat, xend = endLon, yend = endLat,
                    colour = route),
                alpha = 3/4, size = 2, data = legs_df
        ) +
        labs(x = "Longitude", y = "Latitude", colour = "Route")  +
        facet_wrap(~ route, ncol = 3) +
        theme(legend.position = "top")