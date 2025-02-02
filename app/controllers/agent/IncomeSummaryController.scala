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

import config.featureswitch.FeatureSwitching
import config.{AgentItvcErrorHandler, FrontendAppConfig}
import controllers.agent.predicates.ClientConfirmedController
import implicits.ImplicitDateFormatter
import models.liabilitycalculation.viewmodels.IncomeBreakdownViewModel
import models.liabilitycalculation.{LiabilityCalculationError, LiabilityCalculationResponse}
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.mvc._
import services.{CalculationService, IncomeSourceDetailsService}
import uk.gov.hmrc.auth.core.AuthorisedFunctions
import uk.gov.hmrc.play.language.LanguageUtils
import views.html.IncomeBreakdown

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class IncomeSummaryController @Inject()(incomeBreakdown: IncomeBreakdown,
                                        val authorisedFunctions: AuthorisedFunctions,
                                        calculationService: CalculationService,
                                        incomeSourceDetailsService: IncomeSourceDetailsService
                                       )(implicit val appConfig: FrontendAppConfig,
                                         val languageUtils: LanguageUtils,
                                         mcc: MessagesControllerComponents,
                                         implicit val ec: ExecutionContext,
                                         val itvcErrorHandler: AgentItvcErrorHandler)
  extends ClientConfirmedController with ImplicitDateFormatter with FeatureSwitching with I18nSupport {

  def showIncomeSummary(taxYear: Int): Action[AnyContent] = Authenticated.async { implicit request =>
    implicit user =>
      getMtdItUserWithIncomeSources(incomeSourceDetailsService, useCache = true) flatMap { implicit mtdItUser =>
        calculationService.getLiabilityCalculationDetail(getClientMtditid, getClientNino, taxYear).map {
          case liabilityCalc: LiabilityCalculationResponse =>
            val viewModel = IncomeBreakdownViewModel(liabilityCalc.calculation)
            viewModel match {
              case Some(model) => Ok(incomeBreakdown(model, taxYear, backUrl(taxYear), isAgent = true))
              case _ =>
                Logger("application").warn(s"[Agent][IncomeSummaryController][showIncomeSummary[$taxYear]] No income data could be retrieved. Not found")
                itvcErrorHandler.showInternalServerError()
            }
          case error: LiabilityCalculationError if error.status == NOT_FOUND =>
            Logger("application").info(
              s"[Agent][IncomeSummaryController][showIncomeSummary[$taxYear]] No income data found.")
            itvcErrorHandler.showInternalServerError()
          case _: LiabilityCalculationError =>
            Logger("application").error(
              s"[Agent][IncomeSummaryController][showIncomeSummary[$taxYear]] No new calc income data error found. Downstream error")
            itvcErrorHandler.showInternalServerError()
        }
      }
  }

  def backUrl(taxYear: Int): String = controllers.agent.routes.TaxYearOverviewController.show(taxYear).url

}
