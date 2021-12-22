/*
 * Copyright 2021 HM Revenue & Customs
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
import audit.models.HomeAudit
import auth.MtdItUser
import config.featureswitch._
import config.{FrontendAppConfig, ItvcErrorHandler}
import controllers.predicates.{AuthenticationPredicate, IncomeSourceDetailsPredicate, NinoPredicate, SessionTimeoutPredicate}
import models.financialDetails.{FinancialDetailsErrorModel, FinancialDetailsModel, FinancialDetailsResponseModel}
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.mvc._
import play.twirl.api.Html
import services.{FinancialDetailsService, NextUpdatesService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import utils.CurrentDateProvider

import java.time.LocalDate
import implicits.ImplicitDateFormatterImpl
import uk.gov.hmrc.http.InternalServerException

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class HomeController @Inject()(val homeView: views.html.Home,
                               val checkSessionTimeout: SessionTimeoutPredicate,
                               val authenticate: AuthenticationPredicate,
                               val retrieveNino: NinoPredicate,
                               val retrieveIncomeSources: IncomeSourceDetailsPredicate,
                               val nextUpdatesService: NextUpdatesService,
                               val itvcErrorHandler: ItvcErrorHandler,
                               val financialDetailsService: FinancialDetailsService,
                               val currentDateProvider: CurrentDateProvider,
                               auditingService: AuditingService)
                              (implicit val ec: ExecutionContext,
                               mcc: MessagesControllerComponents,
                               val appConfig: FrontendAppConfig,
                               dateFormatter: ImplicitDateFormatterImpl) extends FrontendController(mcc) with I18nSupport with FeatureSwitching {

//  private def view(nextPaymentDueDate: Option[LocalDate], nextUpdate: LocalDate, overDuePaymentsCount: Int,
//                   overDueUpdatesCount: Option[Int],
//                   nextPaymentOrOverdue: Option[Either[(LocalDate, Boolean), Int]],
//                   nextUpdateOrOverdue: Either[(LocalDate, Boolean), Int],
//                   overduePaymentExists: Boolean,
//                   dunningLockExists: Boolean, currentTaxYear: Int)
//                  (implicit user: MtdItUser[_]): Html = {
//    homeView(
//      nextPaymentDueDate = nextPaymentDueDate,
//      nextUpdate = nextUpdate,
//      overDuePaymentsCount = overDuePaymentsCount,
//      overDueUpdatesCount = overDueUpdatesCount,
//      user.saUtr,
//      ITSASubmissionIntegrationEnabled = isEnabled(ITSASubmissionIntegration),
//      nextPaymentOrOverdue = nextPaymentOrOverdue,
//      nextUpdateOrOverdue = nextUpdateOrOverdue,
//      overduePaymentExists = overduePaymentExists,
//      paymentHistoryEnabled = isEnabled(PaymentHistory),
//      implicitDateFormatter = dateFormatter,
//      dunningLockExists = dunningLockExists,
//      currentTaxYear = currentTaxYear
//    )
//  }
  private def dunningLockExists(financialDetailsResponseModel: List[FinancialDetailsResponseModel]): Boolean = {
    financialDetailsResponseModel.collectFirst {
      case fdm: FinancialDetailsModel if fdm.dunningLockExists => true
    }.isDefined
  }

  private def overduePaymentExists(nextPaymentOrOverdue: Option[Either[(LocalDate, Boolean), Int]]): Boolean = {
    nextPaymentOrOverdue match {
      case Some(_@Left((_, true))) => true
      case Some(_@Right(overdueCount)) if overdueCount > 0 => true
      case _ => false
    }
  }

  private def getOverDuePaymentsCount(dueChargesDetails: Option[Either[(LocalDate, Boolean), Int]]): Int = {
    dueChargesDetails match {
      case Some(deets) => deets match {
        case Right(size) => size
        case Left((_, isOverdue)) => if (isOverdue) 1 else 0
      }
      case None => 0
    }
  }

  val home: Action[AnyContent] = (checkSessionTimeout andThen authenticate andThen retrieveNino andThen retrieveIncomeSources).async {
    implicit user =>
      for {
        //        mtdItUser <- getMtdItUserWithIncomeSources(incomeSourceDetailsService, useCache = true)
        dueObligationDetails <- nextUpdatesService.getObligationDueDates()(implicitly, implicitly, user)
        unpaidFinancialDetails <- financialDetailsService.getAllUnpaidFinancialDetails(user, implicitly, implicitly)
        _ = if (unpaidFinancialDetails.exists(fds => fds.isInstanceOf[FinancialDetailsErrorModel]
          && fds.asInstanceOf[FinancialDetailsErrorModel].code != NOT_FOUND))
          throw new InternalServerException("[FinancialDetailsService][getChargeDueDates] - Failed to retrieve successful financial details")
        dueChargesDetails = financialDetailsService.getChargeDueDates(unpaidFinancialDetails)
        dunningLockExistsValue = dunningLockExists(unpaidFinancialDetails)
      } yield {
        if (isEnabled(TxmEventsApproved)) {
          auditingService.extendedAudit(HomeAudit(
            user, dueChargesDetails, dueObligationDetails
          ))
        }
        Ok(homeView(
          nextPaymentDueDate = None,
          nextUpdate = LocalDate.of(2018, 3, 29),
          overDuePaymentsCount = getOverDuePaymentsCount(dueChargesDetails),
          nextPaymentOrOverdue = dueChargesDetails,
          nextUpdateOrOverdue = dueObligationDetails,
          overduePaymentExists = overduePaymentExists(dueChargesDetails),
          overDueUpdatesCount = Some(1),
          utr = user.saUtr,
          ITSASubmissionIntegrationEnabled = isEnabled(ITSASubmissionIntegration),
          paymentHistoryEnabled = isEnabled(PaymentHistory),
          implicitDateFormatter = dateFormatter,
          dunningLockExists = dunningLockExistsValue,
          currentTaxYear = user.incomeSources.getCurrentTaxEndYear,
          isAgent = false
        )(implicitly, implicitly, user, implicitly))
      }
  }
//      nextUpdatesService.getNextDeadlineDueDateAndOverDueObligations(user.incomeSources).flatMap { latestDeadlineDate =>
//
//        val unpaidCharges: Future[List[FinancialDetailsResponseModel]] = financialDetailsService.getAllUnpaidFinancialDetails
//
//        val dueDates: Future[List[LocalDate]] = unpaidCharges.map {
//          _.flatMap {
//            case fdm: FinancialDetailsModel => fdm.validChargesWithRemainingToPay.getAllDueDates
//            case _ => List.empty[LocalDate]
//          }.sortWith(_ isBefore _)
//        }
//
//        val dunningLockExists: Future[Boolean] = unpaidCharges.map {
//          _.collectFirst {
//            case fdm: FinancialDetailsModel if fdm.dunningLockExists => true
//          }.isDefined
//        }
//
////        val nextPaymentOrOverdue = financialDetailsService.getChargeDueDates(unpaidCharges)
//
//        dueDates.map(_.sortBy(_.toEpochDay())).flatMap { paymentsDue =>
//          dunningLockExists.map { dunningLockExistsValue =>
//            val overDuePaymentsCount = paymentsDue.count(_.isBefore(currentDateProvider.getCurrentDate()))
//            val overDueUpdatesCount = latestDeadlineDate._2.size
//
//            if (isEnabled(TxmEventsApproved)) {
//              auditingService.extendedAudit(HomeAudit(
//                mtdItUser = user,
//                paymentsDue.headOption,
//                latestDeadlineDate._1,
//                overDuePaymentsCount,
//                overDueUpdatesCount
//              ))
//            }
//
//            Ok(view(nextPaymentDueDate = paymentsDue.headOption,
//              nextUpdate = latestDeadlineDate._1,
//              overDuePaymentsCount = overDuePaymentsCount,
//              overDueUpdatesCount = Some(overDueUpdatesCount),
//              nextPaymentOrOverdue = None,
//              nextUpdateOrOverdue = Right(2),
//              overduePaymentExists = false,
//              dunningLockExists = dunningLockExistsValue,
//              currentTaxYear = user.incomeSources.getCurrentTaxEndYear))
//          }
//        }
//
//      }.recover {
//        case ex =>
//          Logger("application").error(s"[HomeController][Home] Downstream error, ${ex.getMessage}")
//          itvcErrorHandler.showInternalServerError()
//      }




}
