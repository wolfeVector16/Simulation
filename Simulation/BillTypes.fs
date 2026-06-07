namespace Simulation.Domain

open System

[<Struct>]
type BillId = BillId of Guid

type BillingCycleId = BillingCycleId of int

type BillKind =
    | TaxBill
    | UtilityBill of provider: InstitutionId option
    | RentBill of landlord: InstitutionId option
    | MortgageBill of lender: InstitutionId option
    | FineBill
    | ServiceFeeBill of provider: InstitutionId option
    | PrivateDebtBill of lender: InstitutionId option
    | LateFeeBill of originalKind: BillKind
    | LegacyBill

type BillRecipient =
    | CityRecipient
    | InstitutionRecipient of InstitutionId
    | LandlordRecipient of InstitutionId
    | ExternalFinanceSector
    | ExternalPrivateSector
    | ExternalServiceSector

type BillStatus =
    | BillOpen
    | BillPartiallyPaid
    | BillPaidStatus
    | BillLate
    | BillWrittenOff

type BillCharge =
    { Kind: BillKind
      Amount: decimal }

type HouseholdBill =
    { Id: BillId
      HouseholdId: HouseholdId
      Kind: BillKind
      Amount: decimal
      DueCycle: BillingCycleId option
      CreatedAt: SimTime
      DueAt: SimTime option
      PaidAmount: decimal
      Status: BillStatus }

type ExternalSectorLedger =
    { FinanceOutflow: decimal
      PrivateSectorOutflow: decimal
      ServiceSectorOutflow: decimal }

