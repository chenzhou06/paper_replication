workspace()
using DataFrames
using ReadStat
using DataFramesMeta
using FixedEffectModels

df = DataFrames
dfm = DataFramesMeta
fe = FixedEffectModels


# table 1 (panel A, B and C) descriptive statistics

## table 1 panel A
bdp = ReadStat.read_dta("./original/PUBLIC_DATA/BILATERAL_DISTANCES_PUBLIC.dta")
describe(bdp)

# table 1 panel B
frp_rep = df.readtable("./FREIGHT_RATES_PUBLIC2.csv")
frp = read_dta("./original/PUBLIC_DATA/FREIGHT_RATES_PUBLIC2.dta")
describe(exp(frp[:lfreight]))

cp = ReadStat.read_dta("./original/PUBLIC_DATA/CLIWOC_PUBLIC.dta")

cpd = cp |>
    d -> @transform(d, voj_duration_hours = :voj_duration .* 24,
                    clipper_min =:TIME_5_2_5)  |>
    d -> @dfm.where(d, .!isna.(:voj_duration)) |>
    d -> @dfm.where(d, 1 .< :voj_duration .<= 180, 1*24 .< :clipper_min .<= 180*24)

describe(cpd[:voj_duration])

# table 1 panel C
btp_rep = df.readtable("./BILATERAL_TRADE_PUBLIC2.csv")
btp = read_dta("./original/PUBLIC_DATA/BILATERAL_TRADE_PUBLIC2.dta")
describe(btp[:expr])


# table A.2 list of countries with available bilateral trade data
# skip

# table 2 and figure A3
cpd[:voyagefromp] = df.pool(cpd[:VoyageFrom])
cpd[:voyagetop] = df.pool(cpd[:VoyageTo])
cpd[:yearp] = df.pool(cpd[:year])


fe.reg(cpd, @model(
    voj_duration_hours ~ clipper_min,
    vcov = cluster(voyagefromp + voyagetop + yearp)))


fe.reg(cpd, @model(
    voj_duration_hours ~ clipper_min,
    fe = yearp,
    vcov = cluster(voyagefromp + voyagetop + yearp)
))

fe.reg(cpd, @model(
    voj_duration_hours ~ clipper_min + geo_distance1,
    vcov = cluster(voyagefromp + voyagetop + yearp)
))

fe.reg(cpd,  @model(
    voj_duration_hours ~ clipper_min + geo_distance1,
    fe = yearp,
    vcov = cluster(voyagefromp + voyagetop + yearp)
))

# table 3 gravity regressions
btp[:country_op] = df.pool(btp[:country_o])
btp[:country_dp] = df.pool(btp[:country_d])
btp[:yearp] = df.pool(btp[:year])

filter(names(btp)) do x
    startswith(string(x), "dd6_")
end


fe.reg(btp, @model(
    lexpr ~ dd6_60lsteam + dd6_60lsail2 +dd6_b65lsteam + dd6_b65lsail2 +dd6_b70lsteam + dd6_b70lsail2 +dd6_b75lsteam + dd6_b75lsail2 +dd6_b80lsteam + dd6_b80lsail2 +dd6_b85lsteam + dd6_b85lsail2 +dd6_b90lsteam + dd6_b90lsail2 +dd6_b95lsteam + dd6_b95lsail2 +dd6_b00lsteam + dd6_b00lsail2,
    fe = country_op + country_dp + yearp,
    vcov = cluster(country_op + country_dp + yearp)
))

fe.reg(btp, @model(
    lexpr ~ dd6_60lsteam + dd6_60lsail2 +dd6_b65lsteam + dd6_b65lsail2 +dd6_b70lsteam + dd6_b70lsail2 +dd6_b75lsteam + dd6_b75lsail2 +dd6_b80lsteam + dd6_b80lsail2 +dd6_b85lsteam + dd6_b85lsail2 +dd6_b90lsteam + dd6_b90lsail2 +dd6_b95lsteam + dd6_b95lsail2 +dd6_b00lsteam + dd6_b00lsail2 + ldist,
    fe = country_op + country_dp + yearp,
    vcov = cluster(country_op + country_dp + yearp)
))

fe.reg(btp, @model(
    lexpr ~ dd6_60lsteam + dd6_b65lsteam + dd6_b70lsteam + dd6_b75lsteam + dd6_b80lsteam + dd6_b85lsteam + dd6_b90lsteam + dd6_b95lsteam + dd6_b00lsteam + dd6_60lsail2 + dd6_b65lsail2 + dd6_b70lsail2 + dd6_b75lsail2 + dd6_b80lsail2 + dd6_b85lsail2 + dd6_b90lsail2 + dd6_b95lsail2 + dd6_b00lsail2,
    fe = country_op & yearp + country_dp & yearp,
    vcov = cluster(country_op + country_dp + yearp)
))

fe.reg(btp, @model(
    lexpr ~ dd6_60lsteam + dd6_b65lsteam + dd6_b70lsteam + dd6_b75lsteam + dd6_b80lsteam + dd6_b85lsteam + dd6_b90lsteam + dd6_b95lsteam + dd6_b00lsteam + dd6_60lsail2 + dd6_b65lsail2 + dd6_b70lsail2 + dd6_b75lsail2 + dd6_b80lsail2 + dd6_b85lsail2 + dd6_b90lsail2 + dd6_b95lsail2 + dd6_b00lsail2 + ldist,
    fe = country_op & yearp + country_dp & yearp,
    vcov = cluster(country_op + country_dp + yearp)
))


btp[:pairp] = df.pool(btp[:pair])

fe.reg(btp, @model(
    lexpr ~ dd6_60lsteam + dd6_b65lsteam + dd6_b70lsteam + dd6_b75lsteam + dd6_b80lsteam + dd6_b85lsteam + dd6_b90lsteam + dd6_b95lsteam + dd6_b00lsteam + dd6_60lsail2 + dd6_b65lsail2 + dd6_b70lsail2 + dd6_b75lsail2 + dd6_b80lsail2 + dd6_b85lsail2 + dd6_b90lsail2 + dd6_b95lsail2 + dd6_b00lsail2,
    fe = pairp + yearp,
    vcov = cluster(country_op + country_dp + yearp)
))


# table 4
frp[:destination_countryp] = df.pool(frp[:destination_country])
frp[:origin_countryp] = df.pool(frp[:origin_country])
frp[:yearp] = df.pool(frp[:year])
frp[:product_rev_p] = df.pool(frp[:product_rev_])
frp[:vojagep] = df.pool(frp[:vojage])

fe.reg(frp, @model(
    lfreight ~ dd5_60_lsteam + dd5_70_lsteam + dd5_80_lsteam + dd5_90_lsteam + dd5_00_lsteam + dd5_60_lsail2 + dd5_70_lsail2 + dd5_80_lsail2 + dd5_90_lsail2 + dd5_00_lsail2,
    fe = yearp + product_rev_p + destination_countryp,
    vcov = cluster(destination_countryp + yearp)
))

fe.reg(frp, @model(
    lfreight ~ dd5_60_lsteam + dd5_70_lsteam + dd5_80_lsteam + dd5_90_lsteam + dd5_00_lsteam + dd5_60_lsail2 + dd5_70_lsail2 + dd5_80_lsail2 + dd5_90_lsail2 + dd5_00_lsail2 + ldist,
    fe = yearp + product_rev_p + destination_countryp,
    vcov = cluster(destination_countryp + yearp)
))

fe.reg(frp, @model(
    lfreight ~ dd5_60_lsteam + dd5_70_lsteam + dd5_80_lsteam + dd5_90_lsteam + dd5_00_lsteam + dd5_60_lsail2 + dd5_70_lsail2 + dd5_80_lsail2 + dd5_90_lsail2 + dd5_00_lsail2,
    fe = yearp + product_rev_p + vojagep,
    vcov = cluster(destination_countryp + yearp)
))
