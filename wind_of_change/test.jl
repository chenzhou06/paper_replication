using ReadStat, DataFrames
using Query
using DataFramesMeta
dm = DataFramesMeta

btp = read_dta("./original/PUBLIC_DATA/BILATERAL_TRADE_PUBLIC.dta")

## define variables to be used in gravity regressions
btpln = @dm.transform(
    btp,
    lexpr = log(:expr),
    ldist = log(:geo_dist),
    lsteam = ifelse(
        :year .< 1870,
        log(:TIME_4_2),
        log(:TIME_4_1)
    ),
    lsail2 = ifelse(
        :year .< 1870,
        log(:TIME_5_2_5),
        log(:TIME_5_1_5)
    ),
    pair = ifelse(
        :country_d .> :country_o,
        :country_d .* :country_o,
        :country_o .* :country_d
    ),

    d6_60 = ifelse(:year .<= 1860, 1, 0),
    d6_65 = ifelse((:year .<= 1865) .& (:year .> 1860), 1, 0),
    d6_70 = ifelse((:year .<= 1870) .& (:year .> 1865), 1, 0),
    d6_75 = ifelse((:year .<= 1875) .& (:year .> 1870), 1, 0),
    d6_80 = ifelse((:year .<= 1880) .& (:year .> 1875), 1, 0),
    d6_85 = ifelse((:year .<= 1885) .& (:year .> 1880), 1, 0),
    d6_90 = ifelse((:year .<= 1890) .& (:year .> 1885), 1, 0),
    d6_95 = ifelse((:year .<= 1895) .& (:year .> 1890), 1, 0),
    d6_00 = ifelse((:year .<= 1900) .& (:year .> 1895), 1, 0)
)

btpdd = @dm.transform(
    btpln,
    dd6_60lsteam = :d6_60 .* :lsteam,
    dd6_b65lsteam = :d6_65 .* :lsteam,
    dd6_b70lsteam = :d6_70 .* :lsteam,
    dd6_b75lsteam = :d6_75 .* :lsteam,
    dd6_b80lsteam = :d6_80 .* :lsteam,
    dd6_b85lsteam = :d6_85 .* :lsteam,
    dd6_b90lsteam = :d6_90 .* :lsteam,
    dd6_b95lsteam = :d6_95 .* :lsteam,
    dd6_b00lsteam = :d6_00 .* :lsteam,

    dd6_60lsail2 = :d6_60 .* :lsail2,
    dd6_b65lsail2 = :d6_65 .* :lsail2,
    dd6_b70lsail2 = :d6_70 .* :lsail2,
    dd6_b75lsail2 = :d6_75 .* :lsail2,
    dd6_b80lsail2 = :d6_80 .* :lsail2,
    dd6_b85lsail2 = :d6_85 .* :lsail2,
    dd6_b90lsail2 = :d6_90 .* :lsail2,
    dd6_b95lsail2 = :d6_95 .* :lsail2,
    dd6_b00lsail2 = :d6_00 .* :lsail2
)

writetable("BILATERAL_TRADE_PUBLIC2.csv", btpdd)


## define variables to be used in freight rates regressions
frp = read_dta("./original/PUBLIC_DATA/FREIGHT_RATES_PUBLIC.dta")

frp = @transform(frp, vojage = :origin .* :destination)
frp = @transform(frp, vojage_country = :origin_country .* :destination_country)

frpm = @linq frp |>
    by([:vojage, :year], Tot_Shellings_per_tons = mean(:Tot_Shellings_per_tons))


frpshell = join(frp, frpm, on = [:vojage, :year])


frpln = @linq frpshell |> where((:year .>= 1840) .& (:year .<= 1900)) |>
    transform(lfreight = log(:Tot_Shellings_per_tons)) |>
    transform(lsteam = ifelse( :year .< 1870, log(:STEAM_CLOSED),
    log(:STEAM_OPEN), ), lsail2 = ifelse( :year .< 1870,
    log(:SAIL_MAY_CLOSED), log(:SAIL_MAY_OPEN) ), ldist =
    log(:GEO_DISTANCE))


frpdec = @linq frpln |>
    transform(
        d_60 = ifelse(:year .<= 1860, 1, 0),
        d_70 = ifelse(1860 .< :year .<= 1870, 1, 0),
        d_80 = ifelse(1870 .< :year .<= 1880, 1, 0),
        d_90 = ifelse(1880 .< :year .<= 1890, 1, 0),
        d_00 = ifelse(1890 .< :year .<= 1900, 1, 0)) |>
    transform(
        dd5_60_lsail2 = :d_60 .* :lsail2,
        dd5_70_lsail2 = :d_70 .* :lsail2,
        dd5_80_lsail2 = :d_80 .* :lsail2,
        dd5_90_lsail2 = :d_90 .* :lsail2,
        dd5_00_lsail2 = :d_00 .* :lsail2,

        dd5_60_lsteam = :d_60 .* :lsteam,
        dd5_70_lsteam = :d_70 .* :lsteam,
        dd5_80_lsteam = :d_80 .* :lsteam,
        dd5_90_lsteam = :d_90 .* :lsteam,
        dd5_00_lsteam = :d_00 .* :lsteam,

        decade = broadcast(
            x -> if  x < 1850
            40
            elseif 1850 < x <=1860
            50
            elseif 1860 < x <= 1870
            60
            elseif 1870 < x <= 1880
            70
            elseif 1880 < x <= 1890
            80
            elseif 1890 < x <= 1900
            90
            elseif 1900 < x <= 1910
            100
            end,
            :year
        )
    )

writetable("FREIGHT_RATES_PUBLIC2.csv", frpdec)
