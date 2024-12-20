module Internal.Constants (
  constEscrowAddress,
  constBackdoorAddress,
  constOracleOperatorAddress,
) where

import GeniusYield.Types (GYAddress, GYNetworkId (GYMainnet), unsafeAddressFromText)

constEscrowAddress :: GYNetworkId -> GYAddress
constEscrowAddress GYMainnet = unsafeAddressFromText "addr1qxpk2jharz8704cqncw2yu2l9k4klxz3rh7z3m9cavr94xm4nkl28hmhngyh77w9fjrrcdnkcr98ye5p0qu73c2afvlsmdclgy"
constEscrowAddress _ = unsafeAddressFromText "addr_test1qpyfg6h3hw8ffqpf36xd73700mkhzk2k7k4aam5jeg9zdmj6k4p34kjxrlgugcktj6hzp3r8es2nv3lv3quyk5nmhtqqexpysh"

constBackdoorAddress :: GYNetworkId -> GYAddress
constBackdoorAddress GYMainnet = unsafeAddressFromText "addr1v857xkza0e82ce5nhx9mcwj8eylve9mpwcgl9gse5pc30echsqvjm"
constBackdoorAddress _ =
  unsafeAddressFromText "addr_test1vrvx594erd0d2sj4hm4ux4av3jv5wqu4uayjat44uh8r08cqta7gu"

constOracleOperatorAddress :: GYNetworkId -> GYAddress
constOracleOperatorAddress GYMainnet = unsafeAddressFromText "addr1qxymp067sp2k5kqry0vcsk4et40fuha9gg08kuad4m37rndl9u29prth7gntv9z0ev3zcx4fs65u4agtnh32pw7kmqdqqegjhe"
constOracleOperatorAddress _ = unsafeAddressFromText "addr_test1vrvx594erd0d2sj4hm4ux4av3jv5wqu4uayjat44uh8r08cqta7gu"
