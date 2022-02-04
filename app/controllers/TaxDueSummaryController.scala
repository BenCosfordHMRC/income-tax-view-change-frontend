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

package controllers

import audit.AuditingService
import audit.models._
import auth.MtdItUser
import config.featureswitch.{FeatureSwitching, NewTaxCalcProxy, TxmEventsApproved}
import config.{FrontendAppConfig, ItvcErrorHandler}
import controllers.predicates._
import implicits.ImplicitDateFormatter
import models.calculation._
import models.liabilitycalculation.{LiabilityCalculationError, LiabilityCalculationResponse}
import models.liabilitycalculation.viewmodels._
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.mvc._
import services.CalculationService
import uk.gov.hmrc.play.language.LanguageUtils
import views.html.TaxCalcBreakdown
import views.html.TaxCalcBreakdownNew

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TaxDueSummaryController @Inject()(checkSessionTimeout: SessionTimeoutPredicate,
                                        authenticate: AuthenticationPredicate,
                                        retrieveNino: NinoPredicate,
                                        retrieveIncomeSources: IncomeSourceDetailsPredicate,
                                        calculationService: CalculationService,
                                        itvcErrorHandler: ItvcErrorHandler,
                                        taxCalcBreakdown: TaxCalcBreakdown,
                                        taxCalcBreakdownNew: TaxCalcBreakdownNew,
                                        val auditingService: AuditingService)
                                       (implicit val appConfig: FrontendAppConfig,
                                        val languageUtils: LanguageUtils,
                                        mcc: MessagesControllerComponents,
                                        val executionContext: ExecutionContext) extends BaseController with ImplicitDateFormatter with FeatureSwitching with I18nSupport {

  val action: ActionBuilder[MtdItUser, AnyContent] = (checkSessionTimeout andThen authenticate andThen retrieveNino
    andThen retrieveIncomeSources)


  def showTaxDueSummary(taxYear: Int): Action[AnyContent] = {

    action.async {
      implicit user => {
        if (isEnabled(NewTaxCalcProxy)) {
          calculationService.getLiabilityCalculationDetail(user.mtditid, user.nino, taxYear).map {
            case liabilityCalc: LiabilityCalculationResponse =>
              val viewModel = TaxDueSummaryViewModel(liabilityCalc)
              auditingService.extendedAudit(TaxCalculationDetailsResponseAuditModelNew(user, viewModel, taxYear))
              Ok(taxCalcBreakdownNew(viewModel, taxYear, backUrl(taxYear)))
            case calcErrorResponse: LiabilityCalculationError if calcErrorResponse.status == NOT_FOUND =>
              Logger("application").info("[TaxDueController][showTaxDueSummary] No calculation data returned from downstream. Not Found.")
              itvcErrorHandler.showInternalServerError()
            case _: LiabilityCalculationError =>
              Logger("application").error(
                "[TaxDueController][showTaxDueSummary[" + taxYear +
                  "]] No new calc deductions data error found. Downstream error")
              itvcErrorHandler.showInternalServerError()
          }
        } else {
          calculationService.getCalculationDetail(user.nino, taxYear).flatMap {
            case calcDisplayModel: CalcDisplayModel =>
              if (isEnabled(TxmEventsApproved)) {
                auditingService.extendedAudit(TaxCalculationDetailsResponseAuditModel(user, calcDisplayModel, taxYear))
              }
              Future.successful(Ok(taxCalcBreakdown(calcDisplayModel, taxYear, backUrl(taxYear))))

            case CalcDisplayNoDataFound =>
              Logger("application").warn(
                "[TaxDueController][showTaxDueSummary[" + taxYear +
                  "]] No tax due data could be retrieved. Not found")
              Future.successful(itvcErrorHandler.showInternalServerError())

            case CalcDisplayError =>
              Logger("application").error(
                "[TaxDueController][showTaxDueSummary[" + taxYear +
                  "]] No tax due data could be retrieved. Downstream error")
              Future.successful(itvcErrorHandler.showInternalServerError())
          }
        }
      }
    }
  }

  def backUrl(taxYear: Int): String = controllers.routes.TaxYearOverviewController.renderTaxYearOverviewPage(taxYear).url

}
