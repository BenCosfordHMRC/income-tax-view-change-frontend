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

import audit.mocks.MockAuditingService
import config.ItvcErrorHandler
import config.featureswitch.{CodingOut, FeatureSwitching, NewTaxCalcProxy}
import controllers.predicates.{NinoPredicate, SessionTimeoutPredicate}
import forms.utils.SessionKeys
import mocks.controllers.predicates.{MockAuthenticationPredicate, MockIncomeSourceDetailsPredicateNoCache}
import mocks.services.{MockCalculationService, MockFinancialDetailsService, MockNextUpdatesService}
import models.calculation.CalcOverview
import models.financialDetails.DocumentDetailWithDueDate
import models.liabilitycalculation.viewmodels.TaxYearOverviewViewModel
import models.nextUpdates.{NextUpdatesErrorModel, ObligationsModel}
import org.jsoup.Jsoup
import play.api.http.Status
import play.api.mvc.MessagesControllerComponents
import play.api.test.Helpers._
import testConstants.BaseTestConstants.testMtditid
import testConstants.CalcBreakdownTestConstants.calculationDataSuccessModel
import testConstants.EstimatesTestConstants._
import testConstants.FinancialDetailsTestConstants.{documentDetailClass2Nic, documentDetailPaye, financialDetails, fullDocumentDetailWithDueDateModel}
import testConstants.MessagesLookUp
import testConstants.NewCalcBreakdownUnitTestConstants.liabilityCalculationModelSuccessFull
import testUtils.TestSupport
import views.html.{TaxYearOverview, TaxYearOverviewOld}

import java.time.LocalDate

class TaxYearOverviewControllerSpec extends TestSupport with MockCalculationService
  with MockAuthenticationPredicate with MockIncomeSourceDetailsPredicateNoCache
  with MockFinancialDetailsService with FeatureSwitching
  with MockAuditingService with MockNextUpdatesService {

  val taxYearOverviewView: TaxYearOverview = app.injector.instanceOf[TaxYearOverview]
  val taxYearOverviewViewOld: TaxYearOverviewOld = app.injector.instanceOf[TaxYearOverviewOld]

  object TestTaxYearOverviewController extends TaxYearOverviewController(
    taxYearOverviewView,
    taxYearOverviewViewOld,
    MockAuthenticationPredicate,
    mockCalculationService,
    app.injector.instanceOf[SessionTimeoutPredicate],
    mockFinancialDetailsService,
    app.injector.instanceOf[ItvcErrorHandler],
    MockIncomeSourceDetailsPredicateNoCache,
    app.injector.instanceOf[NinoPredicate],
    mockNextUpdatesService,
    mockAuditingService
  )(appConfig,
    app.injector.instanceOf[MessagesControllerComponents],
    ec)

  lazy val messagesLookUp = new MessagesLookUp.Calculation(testYear)

  val testChargesList: List[DocumentDetailWithDueDate] = List(fullDocumentDetailWithDueDateModel.copy(
    dueDate = fullDocumentDetailWithDueDateModel.documentDetail.interestEndDate, isLatePaymentInterest = true),
    fullDocumentDetailWithDueDateModel)
  val testEmptyChargesList: List[DocumentDetailWithDueDate] = List.empty
  val class2NicsChargesList: List[DocumentDetailWithDueDate] = List(documentDetailClass2Nic)
  val payeChargesList: List[DocumentDetailWithDueDate] = List(documentDetailPaye)
  val testObligtionsModel: ObligationsModel = ObligationsModel(Nil)
  val taxYearsRefererBackLink: String = "http://www.somedomain.org/report-quarterly/income-and-expenses/view/tax-years"
  val taxYearsBackLink: String = "/report-quarterly/income-and-expenses/view/tax-years"
  val homeBackLink: String = "/report-quarterly/income-and-expenses/view"

  "The TaxYearOverview.renderTaxYearOverviewPage(year) action when NewTaxCalcProxy is disabled" when {
    "all calls are returned correctly" should {
      "show the Tax Year Overview Page" in {
        disable(NewTaxCalcProxy)
        mockSingleBusinessIncomeSource()
        mockCalculationSuccess()
        mockFinancialDetailsSuccess()
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )

        val calcOverview: CalcOverview = CalcOverview(calculationDataSuccessModel)
        val expectedContent: String = taxYearOverviewViewOld(
          testYear,
          Some(calcOverview),
          testChargesList,
          testObligtionsModel,
          taxYearsBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsRefererBackLink))

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
        contentType(result) shouldBe Some("text/html")
        result.futureValue.session.get(SessionKeys.chargeSummaryBackPage) shouldBe Some("taxYearOverview")
      }
    }

    "all calls are returned correctly and Referer was a Home page" should {
      "show the Tax Year Overview Page and back link should be to the Home page" in {
        disable(NewTaxCalcProxy)
        mockSingleBusinessIncomeSource()
        mockCalculationSuccess()
        mockFinancialDetailsSuccess()
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )

        val calcOverview: CalcOverview = CalcOverview(calculationDataSuccessModel)
        val expectedContent: String = taxYearOverviewViewOld(
          testYear,
          Some(calcOverview),
          testChargesList,
          testObligtionsModel,
          homeBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveAndRefererToHomePage)

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
        contentType(result) shouldBe Some("text/html")
        result.futureValue.session.get(SessionKeys.chargeSummaryBackPage) shouldBe Some("taxYearOverview")
      }
    }

    "the coding out feature switch is enabled" should {
      "include Class 2 Nics in the charges list when Class 2 Nics is present" in {
        disable(NewTaxCalcProxy)
        enable(CodingOut)

        mockSingleBusinessIncomeSource()
        mockCalculationSuccess()
        mockFinancialDetailsSuccess(
          financialDetailsModelResponse = financialDetails(
            documentDetails = documentDetailClass2Nic.documentDetail
          )
        )
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )

        val calcOverview: CalcOverview = CalcOverview(calculationDataSuccessModel)
        val expectedContent: String = taxYearOverviewViewOld(
          testYear,
          Some(calcOverview),
          class2NicsChargesList,
          testObligtionsModel,
          taxYearsBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
      }

      "include Paye in the charges list when Paye is present" in {
        disable(NewTaxCalcProxy)
        enable(CodingOut)

        mockSingleBusinessIncomeSource()
        mockCalculationSuccess()
        mockFinancialDetailsSuccess(
          financialDetailsModelResponse = financialDetails(
            documentDetails = documentDetailPaye.documentDetail
          )
        )
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )

        val calcOverview: CalcOverview = CalcOverview(calculationDataSuccessModel)
        val expectedContent: String = taxYearOverviewViewOld(
          testYear,
          Some(calcOverview),
          payeChargesList,
          testObligtionsModel,
          taxYearsBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
      }
    }

    "the coding out feature switch is disabled" should {
      "not include Class 2 Nics in the charges list when Class 2 Nics is present" in {
        disable(NewTaxCalcProxy)
        disable(CodingOut)

        mockSingleBusinessIncomeSource()
        mockCalculationSuccess()
        mockFinancialDetailsSuccess(
          financialDetailsModelResponse = financialDetails(
            documentDetails = documentDetailClass2Nic.documentDetail
          )
        )
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )

        val calcOverview: CalcOverview = CalcOverview(calculationDataSuccessModel)
        val expectedContent: String = taxYearOverviewViewOld(
          testYear,
          Some(calcOverview),
          testEmptyChargesList,
          testObligtionsModel,
          taxYearsBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
      }

      "not include Paye in the charges list when Paye is present" in {
        disable(NewTaxCalcProxy)
        disable(CodingOut)

        mockSingleBusinessIncomeSource()
        mockCalculationSuccess()
        mockFinancialDetailsSuccess(
          financialDetailsModelResponse = financialDetails(
            documentDetails = documentDetailPaye.documentDetail
          )
        )
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )

        val calcOverview: CalcOverview = CalcOverview(calculationDataSuccessModel)
        val expectedContent: String = taxYearOverviewViewOld(
          testYear,
          Some(calcOverview),
          testEmptyChargesList,
          testObligtionsModel,
          taxYearsBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
      }
    }

      s"getFinancialDetails returns a $NOT_FOUND" should {
        "show the Tax Year Overview Page" in {
          disable(NewTaxCalcProxy)
          mockSingleBusinessIncomeSource()
          mockCalculationSuccess()
          mockFinancialDetailsNotFound()
          mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
            toDate = LocalDate.of(testYear, 4, 5))(
            response = testObligtionsModel
          )


          val calcOverview: CalcOverview = CalcOverview(calculationDataSuccessModel)
          val expectedContent: String = taxYearOverviewViewOld(
            testYear,
            Some(calcOverview),
            testEmptyChargesList,
            testObligtionsModel,
            taxYearsBackLink,
            codingOutEnabled = true
          ).toString

          val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

          status(result) shouldBe Status.OK
          contentAsString(result) shouldBe expectedContent
          contentType(result) shouldBe Some("text/html")
          result.futureValue.session.get(SessionKeys.chargeSummaryBackPage) shouldBe Some("taxYearOverview")
        }
      }

      "getFinancialDetails returns an error" should {
        "show the internal server error page" in {
          disable(NewTaxCalcProxy)
          mockSingleBusinessIncomeSource()
          mockCalculationCrystalisationSuccess()
          mockFinancialDetailsFailed()

          val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSession)

          status(result) shouldBe Status.INTERNAL_SERVER_ERROR
          contentType(result) shouldBe Some("text/html")
        }
      }

      "getNextUpdates returns an error" should {
        "show the internal server error page" in {
          disable(NewTaxCalcProxy)
          mockSingleBusinessIncomeSource()
          mockCalculationCrystalisationSuccess()
          mockFinancialDetailsNotFound()
          mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
            toDate = LocalDate.of(testYear, 4, 5))(
            response = NextUpdatesErrorModel(500, "INTERNAL_SERVER_ERROR")
          )

          val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSession)

          status(result) shouldBe Status.INTERNAL_SERVER_ERROR
          contentType(result) shouldBe Some("text/html")
        }
      }

      "Called with an Unauthenticated User" should {
        "return redirect SEE_OTHER (303)" in {
          disable(NewTaxCalcProxy)
          setupMockAuthorisationException()

          val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSession)

          status(result) shouldBe Status.SEE_OTHER
        }
      }

      "Called with an Authenticated HMRC-MTD-IT User" when {
        "provided with a negative tax year" should {
          "return Internal Service Error (500)" in {
            disable(NewTaxCalcProxy)
            mockPropertyIncomeSource()

            val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(-testYear)(fakeRequestWithActiveSession)

            status(result) shouldBe Status.INTERNAL_SERVER_ERROR
          }
        }

        "the calculation returned from the calculation service was not found" should {
          "show tax year overview page with expected content" in {
            disable(NewTaxCalcProxy)
            mockSingleBusinessIncomeSource()
            mockCalculationNotFound()
            mockFinancialDetailsSuccess()
            mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
              toDate = LocalDate.of(testYear, 4, 5))(
              response = testObligtionsModel
            )

            val expectedContent: String = taxYearOverviewViewOld(
              testYear,
              None,
              testChargesList,
              testObligtionsModel,
              taxYearsBackLink,
              codingOutEnabled = true
            ).toString

            val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

            status(result) shouldBe Status.OK
            contentAsString(result) shouldBe expectedContent
            contentType(result) shouldBe Some("text/html")
            result.futureValue.session.get(SessionKeys.chargeSummaryBackPage) shouldBe Some("taxYearOverview")
          }
        }

        "the calculation returned from the calculation service was an error" should {
          "return the internal server error page" in {
            disable(NewTaxCalcProxy)
            mockSingleBusinessIncomeSource()
            mockCalculationError()

            val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSession)

            status(result) shouldBe Status.INTERNAL_SERVER_ERROR
            contentType(result) shouldBe Some("text/html")
          }
        }
      }
  }
  "The TaxYearOverview.renderTaxYearOverviewPage(year) action when NewTaxCalcProxy is enabled" when {
    "all calls are returned correctly" should {
      "show the Tax Year Overview Page" in {
        enable(NewTaxCalcProxy)
        mockSingleBusinessIncomeSource()
        mockCalculationSuccessFullNew(testMtditid)
        mockFinancialDetailsSuccess()
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )

        val calcOverview: TaxYearOverviewViewModel = TaxYearOverviewViewModel(liabilityCalculationModelSuccessFull)
        val expectedContent: String = taxYearOverviewView(
          testYear,
          Some(calcOverview),
          testChargesList,
          testObligtionsModel,
          taxYearsBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
        contentType(result) shouldBe Some("text/html")
        result.futureValue.session.get(SessionKeys.chargeSummaryBackPage) shouldBe Some("taxYearOverview")
      }
    }

    "all calls are returned correctly and Referer was a Home page" should {
      "show the Tax Year Overview Page and back link should be to the Home page" in {
        enable(NewTaxCalcProxy)
        mockSingleBusinessIncomeSource()
        mockCalculationSuccessFullNew(testMtditid)
        mockFinancialDetailsSuccess()
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )

        val calcOverview: TaxYearOverviewViewModel = TaxYearOverviewViewModel(liabilityCalculationModelSuccessFull)
        val expectedContent: String = taxYearOverviewView(
          testYear,
          Some(calcOverview),
          testChargesList,
          testObligtionsModel,
          homeBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveAndRefererToHomePage)

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
        contentType(result) shouldBe Some("text/html")
        result.futureValue.session.get(SessionKeys.chargeSummaryBackPage) shouldBe Some("taxYearOverview")
      }
    }

    "the coding out feature switch is enabled" should {
      "include Class 2 Nics in the charges list when Class 2 Nics is present" in {
        enable(NewTaxCalcProxy)
        enable(CodingOut)

        mockSingleBusinessIncomeSource()
        mockCalculationSuccessFullNew(testMtditid)
        mockFinancialDetailsSuccess(
          financialDetailsModelResponse = financialDetails(
            documentDetails = documentDetailClass2Nic.documentDetail
          )
        )
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )

        val calcOverview: TaxYearOverviewViewModel = TaxYearOverviewViewModel(liabilityCalculationModelSuccessFull)
        val expectedContent: String = taxYearOverviewView(
          testYear,
          Some(calcOverview),
          class2NicsChargesList,
          testObligtionsModel,
          taxYearsBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
      }

      "include Paye in the charges list when Paye is present" in {
        enable(NewTaxCalcProxy)
        enable(CodingOut)

        mockSingleBusinessIncomeSource()
        mockCalculationSuccessFullNew(testMtditid)
        mockFinancialDetailsSuccess(
          financialDetailsModelResponse = financialDetails(
            documentDetails = documentDetailPaye.documentDetail
          )
        )
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )

        val calcOverview: TaxYearOverviewViewModel = TaxYearOverviewViewModel(liabilityCalculationModelSuccessFull)
        val expectedContent: String = taxYearOverviewView(
          testYear,
          Some(calcOverview),
          payeChargesList,
          testObligtionsModel,
          taxYearsBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
      }
    }

    "the coding out feature switch is disabled" should {
      "not include Class 2 Nics in the charges list when Class 2 Nics is present" in {
        enable(NewTaxCalcProxy)
        disable(CodingOut)

        mockSingleBusinessIncomeSource()
        mockCalculationSuccessFullNew(testMtditid)
        mockFinancialDetailsSuccess(
          financialDetailsModelResponse = financialDetails(
            documentDetails = documentDetailClass2Nic.documentDetail
          )
        )
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )

        val calcOverview: TaxYearOverviewViewModel = TaxYearOverviewViewModel(liabilityCalculationModelSuccessFull)
        val expectedContent: String = taxYearOverviewView(
          testYear,
          Some(calcOverview),
          testEmptyChargesList,
          testObligtionsModel,
          taxYearsBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
      }

      "not include Paye in the charges list when Paye is present" in {
        enable(NewTaxCalcProxy)
        disable(CodingOut)

        mockSingleBusinessIncomeSource()
        mockCalculationSuccessFullNew(testMtditid)
        mockFinancialDetailsSuccess(
          financialDetailsModelResponse = financialDetails(
            documentDetails = documentDetailPaye.documentDetail
          )
        )
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )

        val calcOverview: TaxYearOverviewViewModel = TaxYearOverviewViewModel(liabilityCalculationModelSuccessFull)
        val expectedContent: String = taxYearOverviewView(
          testYear,
          Some(calcOverview),
          testEmptyChargesList,
          testObligtionsModel,
          taxYearsBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
      }
    }

    s"getFinancialDetails returns a $NOT_FOUND" should {
      "show the Tax Year Overview Page" in {
        enable(NewTaxCalcProxy)
        mockSingleBusinessIncomeSource()
        mockCalculationSuccessFullNew(testMtditid)
        mockFinancialDetailsNotFound()
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = testObligtionsModel
        )


        val calcOverview: TaxYearOverviewViewModel = TaxYearOverviewViewModel(liabilityCalculationModelSuccessFull)
        val expectedContent: String = taxYearOverviewView(
          testYear,
          Some(calcOverview),
          testEmptyChargesList,
          testObligtionsModel,
          taxYearsBackLink,
          codingOutEnabled = true
        ).toString

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

        status(result) shouldBe Status.OK
        contentAsString(result) shouldBe expectedContent
        contentType(result) shouldBe Some("text/html")
        result.futureValue.session.get(SessionKeys.chargeSummaryBackPage) shouldBe Some("taxYearOverview")
      }
    }

    "getFinancialDetails returns an error" should {
      "show the internal server error page" in {
        enable(NewTaxCalcProxy)
        mockSingleBusinessIncomeSource()
        mockFinancialDetailsFailed()

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSession)

        status(result) shouldBe Status.INTERNAL_SERVER_ERROR
        contentType(result) shouldBe Some("text/html")
      }
    }

    "getNextUpdates returns an error" should {
      "show the internal server error page" in {
        enable(NewTaxCalcProxy)
        mockSingleBusinessIncomeSource()
        mockFinancialDetailsNotFound()
        mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
          toDate = LocalDate.of(testYear, 4, 5))(
          response = NextUpdatesErrorModel(500, "INTERNAL_SERVER_ERROR")
        )

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSession)

        status(result) shouldBe Status.INTERNAL_SERVER_ERROR
        contentType(result) shouldBe Some("text/html")
      }
    }

    "Called with an Unauthenticated User" should {
      "return redirect SEE_OTHER (303)" in {
        enable(NewTaxCalcProxy)
        setupMockAuthorisationException()

        val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSession)

        status(result) shouldBe Status.SEE_OTHER
      }
    }

    "Called with an Authenticated HMRC-MTD-IT User" when {
      "provided with a negative tax year" should {
        "return Internal Service Error (500)" in {
          enable(NewTaxCalcProxy)
          mockPropertyIncomeSource()

          val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(-testYear)(fakeRequestWithActiveSession)

          status(result) shouldBe Status.INTERNAL_SERVER_ERROR
        }
      }

      "the calculation returned from the calculation service was not found" should {
        "show tax year overview page with expected content" in {
          enable(NewTaxCalcProxy)
          mockSingleBusinessIncomeSource()
          mockCalculationNotFoundNew(testMtditid)
          mockFinancialDetailsSuccess()
          mockgetNextUpdates(fromDate = LocalDate.of(testYear - 1, 4, 6),
            toDate = LocalDate.of(testYear, 4, 5))(
            response = testObligtionsModel
          )

          val expectedContent: String =  Jsoup.parse(taxYearOverviewViewOld(
            testYear,
            None,
            testChargesList,
            testObligtionsModel,
            taxYearsBackLink,
            codingOutEnabled = true
          ).toString()).text()

          val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSessionWithReferer(referer = taxYearsBackLink))

          status(result) shouldBe Status.OK
          Jsoup.parse(contentAsString(result)).text() shouldBe expectedContent
          contentType(result) shouldBe Some("text/html")
          result.futureValue.session.get(SessionKeys.chargeSummaryBackPage) shouldBe Some("taxYearOverview")
        }
      }

      "the calculation returned from the calculation service was an error" should {
        "return the internal server error page" in {
          enable(NewTaxCalcProxy)
          mockSingleBusinessIncomeSource()
          mockCalculationErrorNew(testMtditid)

          val result = TestTaxYearOverviewController.renderTaxYearOverviewPage(testYear)(fakeRequestWithActiveSession)

          status(result) shouldBe Status.INTERNAL_SERVER_ERROR
          contentType(result) shouldBe Some("text/html")
        }
      }
    }
  }
}
