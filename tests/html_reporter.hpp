////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file html_reporter.hpp
/// \author Jamie Terry
/// \date 2017/04/06
/// \brief Contains implementation for a catch reporter which outputs a html report
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_TEST_HTML_REPORTER_HPP
#define XEN_TEST_HTML_REPORTER_HPP

/*
#include <catch.hpp>

namespace Catch{

	class HtmlReporter : public CumulativeReporterBase {
	public:

		HtmlReporter( ReporterConfig const& _config )
			: CumulativeReporterBase( _config ) {
			// no-op
		}

		virtual ~HtmlReporter(){}

		static std::string getDescription(){
			return "Generates HTML file viewable in a browser";
		}

		virtual void noMatchingTestCases(std::string const& spec){
			printf("No matching test cases: %s\n", spec.c_str());
		}

		virtual void testRunStarting( TestRunInfo const& runInfo ) CATCH_OVERRIDE {
			printf("Test run starting\n");
            // .name :: string (xen_unit_tests)
        }

		virtual void testRunEndedCumulative(){
			printf("Test run ended cumlative\n");
		}

		virtual void testGroupStarting( GroupInfo const& groupInfo ) CATCH_OVERRIDE {
			printf("Test group starting\n");
            // .name :: string (xen_unit_tests)
            // groupIndex :: int
            // groupsCounts :: int
		}

		virtual void testGroupEnded( TestGroupStats const& testGroupStats ) CATCH_OVERRIDE {
			printf("Test group ended\n");
            // .lineInfo
            //    .file :: String
            //    .line :: int
            // .name :: String (Vec2r)
            // .tags :: vector<string> ?other list type
            // .className :: String
            // .description :: String
		}

		virtual void testCaseStarting( TestCaseInfo const& testCaseInfo ) CATCH_OVERRIDE {
			printf("Test case starting\n");
		}

	    virtual void testCaseEnded( TestCaseStats const& testCaseStats ) CATCH_OVERRIDE {
		    printf("Test case ending\n");
            // .testInfo :: TestCaseInfo
            //   .lineInfo
            //      .file :: String
            //      .line :: int
            //   .name :: String (Vec2r)
            //   .tags :: vector<string> ?other list type
            //   .className :: String
            //   .description :: String
            // .totals
            //   .assertions
            //     .failed
            //     .failedButOkay
            //     .passed
            //   .testCases
            //     ...as above...
            // .stdOut :: String
            // .stdErr :: String
            // .aborting :: bool
	    }

		virtual bool assertionEnded( AssertionStats const& assertionStats ) CATCH_OVERRIDE {
			//printf("Assertion ending\n");
            // .totals (...as above...)
            // .infoMessages (never set... empty vector?)
            // .assertionResult
            //    .m_info
            //      .capturedExpression :: String
            //      .lineInfo
            //      .macroName
            //    .m_resultData
            //      ....
			return true;
		}
	};

	INTERNAL_CATCH_REGISTER_REPORTER( "html", HtmlReporter )
} // end namespace Catch

*/
#endif
