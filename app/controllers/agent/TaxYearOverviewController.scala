/*
 * Copyright 2022 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package controllers.agent

import audit.AuditingService
import audit.models.TaxYearOverviewResponseAuditModel
import auth.MtdItUser
import config.featureswitch.{CodingOut, FeatureSwitching, NewTaxCalcProxy}
import config.{AgentItvcErrorHandler, FrontendAppConfig}
import controllers.agent.predicates.ClientConfirmedController
import controllers.agent.utils.SessionKeys
import implicits.ImplicitDateFormatter
import models.calculation.{CalcDisplayModel, CalcDisplayNoDataFound, CalcOverview, Calculation}
import models.financialDetails.{DocumentDetailWithDueDate, FinancialDetailsErrorModel, FinancialDetailsModel}
import models.liabilitycalculation.viewmodels.TaxYearOverviewViewModel
import models.liabilitycalculation.{LiabilityCalculationError, LiabilityCalculationResponse, LiabilityCalculationResponseModel}
import models.nextUpdates.ObligationsModel
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.mvc._
import services.{CalculationService, FinancialDetailsService, IncomeSourceDetailsService, NextUpdatesService}
import uk.gov.hmrc.auth.core.AuthorisedFunctions
import uk.gov.hmrc.play.language.LanguageUtils
import views.html.{TaxYearOverview, TaxYearOverviewOld}

import java.time.LocalDate
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class TaxYearOverviewController @Inject()(taxYearOverview: TaxYearOverview,
                                          taxYearOverviewOld: TaxYearOverviewOld,
                                          val authorisedFunctions: AuthorisedFunctions,
                                          calculationService: CalculationService,
                                          financialDetailsService: FinancialDetailsService,
                                          incomeSourceDetailsService: IncomeSourceDetailsService,
                                          nextUpdatesService: NextUpdatesService,
                                          auditingService: AuditingService
                                         )(implicit val appConfig: FrontendAppConfig,
                                           val languageUtils: LanguageUtils,
                                           mcc: MessagesControllerComponents,
                                           implicit val ec: ExecutionContext,
                                           val itvcErrorHandler: AgentItvcErrorHandler)
  extends ClientConfirmedController with ImplicitDateFormatter with FeatureSwitching with I18nSupport {

  lazy val backAgentTaxYearsUrl: String = controllers.agent.routes.TaxYearsController.show().url
  lazy val backAgentHomeUrl: String = controllers.agent.routes.HomeController.show().url

  def show(taxYear: Int): Action[AnyContent] = Authenticated.async { implicit request =>
    implicit user =>
      getMtdItUserWithIncomeSources(incomeSourceDetailsService, useCache = false) flatMap { implicit mtdItUser =>
        withTaxYearFinancials(taxYear) { documentDetailsWithDueDates =>
          withObligationsModel(taxYear) { obligations =>
            if (isEnabled(NewTaxCalcProxy)) {
              calculationService.getLiabilityCalculationDetail(getClientMtditid, getClientNino, taxYear).map { liabilityCalcResponse =>
                view(liabilityCalcResponse, documentDetailsWithDueDates, taxYear, obligations, getBackURL(request.headers.get(REFERER)))
                  .addingToSession(SessionKeys.chargeSummaryBackPage -> "taxYearOverview")(request)
              }
            }
            else {
              withCalculation(getClientNino(request), taxYear) { calculationOpt =>
                auditingService.extendedAudit(TaxYearOverviewResponseAuditModel(
                  mtdItUser, calculationOpt,
                  documentDetailsWithDueDates, obligations))
                Future.successful(Ok(
                  taxYearOverviewOld(
                    taxYear = taxYear,
                    overviewOpt = calculationOpt.map(calc => CalcOverview(calc)),
                    charges = documentDetailsWithDueDates,
                    obligations = obligations,
                    codingOutEnabled = isEnabled(CodingOut),
                    backUrl = getBackURL(request.headers.get(REFERER)),
                    isAgent = true
                  )
                ).addingToSession(SessionKeys.chargeSummaryBackPage -> "taxYearOverview")(request))
              }
            }
          }
        }
      }
  }

  private def getBackURL(referer: Option[String]): String = {
    referer.map(_.contains(backAgentHomeUrl)) match {
      case Some(true) => backAgentHomeUrl
      case _ => backAgentTaxYearsUrl
    }
  }

  private def view(liabilityCalc: LiabilityCalculationResponseModel,
                   documentDetailsWithDueDates: List[DocumentDetailWithDueDate],
                   taxYear: Int,
                   obligations: ObligationsModel,
                   backUrl: String
                  )(implicit mtdItUser: MtdItUser[_]): Result = {
    liabilityCalc match {
      case liabilityCalc: LiabilityCalculationResponse =>
        val taxYearOverviewViewModel: TaxYearOverviewViewModel = TaxYearOverviewViewModel(liabilityCalc)
        auditingService.extendedAudit(TaxYearOverviewResponseAuditModel(
          mtdItUser, None,
          documentDetailsWithDueDates, obligations, Some(taxYearOverviewViewModel), true))

        Logger("application").info(
          s"[Agent][TaxYearOverviewController][view][$taxYear]] Rendered Tax year overview page with Calc data")

        Ok(taxYearOverview(
          taxYear = taxYear,
          overviewOpt = Some(taxYearOverviewViewModel),
          charges = documentDetailsWithDueDates,
          obligations = obligations,
          codingOutEnabled = isEnabled(CodingOut),
          backUrl = backUrl,
          isAgent = true
        ))
      case error: LiabilityCalculationError if error.status == NOT_FOUND =>
        auditingService.extendedAudit(TaxYearOverviewResponseAuditModel(
          mtdItUser, None,
          documentDetailsWithDueDates, obligations, None, true))

        Logger("application").info(
          s"[Agent][TaxYearOverviewController][view][$taxYear]] Rendered Tax year overview page with No Calc data")

        Ok(taxYearOverview(
          taxYear = taxYear,
          overviewOpt = None,
          charges = documentDetailsWithDueDates,
          obligations = obligations,
          codingOutEnabled = isEnabled(CodingOut),
          backUrl = backUrl,
          isAgent = true
        ))
      case _: LiabilityCalculationError =>
        Logger("application").error(
          s"[Agent][TaxYearOverviewController][view][$taxYear]] No new calc deductions data error found. Downstream error")
        itvcErrorHandler.showInternalServerError()
    }
  }

  private def withCalculation(nino: String, taxYear: Int)(f: Option[Calculation] => Future[Result])(implicit user: MtdItUser[_]): Future[Result] = {
    calculationService.getCalculationDetail(nino, taxYear) flatMap {
      case CalcDisplayModel(_, _, calculation, _) => f(Some(calculation))
      case CalcDisplayNoDataFound => f(None)
      case _ =>
        Logger("application").error(s"[TaxYearOverviewController][withCalculation] - Could not retrieve calculation for year: $taxYear")
        Future.successful(itvcErrorHandler.showInternalServerError())
    }
  }

  private def withTaxYearFinancials(taxYear: Int)(f: List[DocumentDetailWithDueDate] => Future[Result])(implicit user: MtdItUser[_]): Future[Result] = {
    financialDetailsService.getFinancialDetails(taxYear, user.nino) flatMap {
      case financialDetails@FinancialDetailsModel(_, documentDetails, _) =>
        val docDetailsNoPayments = documentDetails.filter(_.paymentLot.isEmpty)
        val docDetailsCodingOut = docDetailsNoPayments.filter(_.isCodingOutDocumentDetail(isEnabled(CodingOut)))
        val documentDetailsWithDueDates: List[DocumentDetailWithDueDate] = {
          docDetailsNoPayments.filter(_.isNotCodingOutDocumentDetail).filter(_.originalAmountIsNotZero).map(
            documentDetail => DocumentDetailWithDueDate(documentDetail, financialDetails.getDueDateFor(documentDetail),
              dunningLock = financialDetails.dunningLockExists(documentDetail.transactionId)))
        }
        val documentDetailsWithDueDatesForLpi: List[DocumentDetailWithDueDate] = {
          docDetailsNoPayments.filter(_.latePaymentInterestAmount.isDefined).filter(_.latePaymentInterestAmountIsNotZero).map(
            documentDetail => DocumentDetailWithDueDate(documentDetail, documentDetail.interestEndDate, isLatePaymentInterest = true,
              dunningLock = financialDetails.dunningLockExists(documentDetail.transactionId)))
        }
        val documentDetailsWithDueDatesCodingOutPaye: List[DocumentDetailWithDueDate] = {
          docDetailsCodingOut.filter(dd => dd.isPayeSelfAssessment && dd.amountCodedOutIsNotZero).map(
            documentDetail => DocumentDetailWithDueDate(documentDetail, financialDetails.getDueDateFor(documentDetail),
              dunningLock = financialDetails.dunningLockExists(documentDetail.transactionId)))
        }
        val documentDetailsWithDueDatesCodingOut: List[DocumentDetailWithDueDate] = {
          docDetailsCodingOut.filter(dd => !dd.isPayeSelfAssessment && dd.originalAmountIsNotZero).map(
            documentDetail => DocumentDetailWithDueDate(documentDetail, financialDetails.getDueDateFor(documentDetail),
              dunningLock = financialDetails.dunningLockExists(documentDetail.transactionId)))
        }
        f(documentDetailsWithDueDates ++ documentDetailsWithDueDatesForLpi ++ documentDetailsWithDueDatesCodingOutPaye ++ documentDetailsWithDueDatesCodingOut)
      case FinancialDetailsErrorModel(NOT_FOUND, _) => f(List.empty)
      case _ =>
        Logger("application").error(s"[TaxYearOverviewController][withTaxYearFinancials] - Could not retrieve financial details for year: $taxYear")
        Future.successful(itvcErrorHandler.showInternalServerError())
    }
  }

  private def withObligationsModel(taxYear: Int)(f: ObligationsModel => Future[Result])(implicit user: MtdItUser[_]): Future[Result] = {
    nextUpdatesService.getNextUpdates(
      fromDate = LocalDate.of(taxYear - 1, 4, 6),
      toDate = LocalDate.of(taxYear, 4, 5)
    ) flatMap {
      case obligationsModel: ObligationsModel => f(obligationsModel)
      case _ =>
        Logger("application").error(s"[TaxYearOverviewController][withObligationsModel] - Could not retrieve obligations for year: $taxYear")
        Future.successful(itvcErrorHandler.showInternalServerError())
    }
  }

}
