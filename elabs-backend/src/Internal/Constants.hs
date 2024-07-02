module Internal.Constants (
  constEscrowAddress,
  constBackdoorAddress,
  constOracleOperatorAddress,
) where

import GeniusYield.Types (GYAddress, GYNetworkId, unsafeAddressFromText)

-- TODO: Use valid escrow address for mainnet
constEscrowAddress :: GYNetworkId -> GYAddress
constEscrowAddress _ = unsafeAddressFromText "addr_test1qpyfg6h3hw8ffqpf36xd73700mkhzk2k7k4aam5jeg9zdmj6k4p34kjxrlgugcktj6hzp3r8es2nv3lv3quyk5nmhtqqexpysh"

-- TODO: Use valid backdoor address for mainnet
constBackdoorAddress :: GYNetworkId -> GYAddress
constBackdoorAddress _ =
  unsafeAddressFromText "addr_test1vrvx594erd0d2sj4hm4ux4av3jv5wqu4uayjat44uh8r08cqta7gu"

constOracleOperatorAddress :: GYNetworkId -> GYAddress
constOracleOperatorAddress _ = unsafeAddressFromText "addr_test1vrvx594erd0d2sj4hm4ux4av3jv5wqu4uayjat44uh8r08cqta7gu"
