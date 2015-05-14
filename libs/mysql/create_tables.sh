-- MySQL dump 10.10
--
-- Host: localhost    Database: astronomy
-- ------------------------------------------------------
-- Server version	5.0.13-rc-standard

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `filters`
--

DROP TABLE IF EXISTS `filters`;
CREATE TABLE `filters` (
  `Band` char(30) NOT NULL,
  `IsophotalWavelength` float default NULL,
  `IsophotalFrequency` float default NULL,
  `Bandwidth` float default NULL,
  `Zeropoint_Jy` float default NULL,
  `Zeropoint_Jy_error` float default NULL,
  `Comment` varchar(80) default NULL,
  PRIMARY KEY  (`Band`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `filters`
--


/*!40000 ALTER TABLE `filters` DISABLE KEYS */;
LOCK TABLES `filters` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `filters` ENABLE KEYS */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- MySQL dump 10.10
--
-- Host: localhost    Database: astronomy
-- ------------------------------------------------------
-- Server version	5.0.13-rc-standard

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `observations`
--

DROP TABLE IF EXISTS `observations`;
CREATE TABLE `observations` (
  `Name` char(30) NOT NULL,
  `Instrument` varchar(30) NOT NULL default '',
  `Date` date NOT NULL,
  `Band` char(20) NOT NULL default '',
  `Comments` text,
  `Exptime` float default NULL,
  `Directory` varchar(80) default '',
  `ReductionCmd` text,
  `NoiseLevel` float default NULL,
  PRIMARY KEY  (`Name`,`Date`,`Band`,`Instrument`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `observations`
--


/*!40000 ALTER TABLE `observations` DISABLE KEYS */;
LOCK TABLES `observations` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `observations` ENABLE KEYS */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- MySQL dump 10.10
--
-- Host: localhost    Database: astronomy
-- ------------------------------------------------------
-- Server version	5.0.13-rc-standard

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `otherproperties`
--

DROP TABLE IF EXISTS `otherproperties`;
CREATE TABLE `otherproperties` (
  `Name` char(30) NOT NULL,
  `Property` varchar(30) NOT NULL,
  `Reference` varchar(80) NOT NULL,
  `Value` varchar(30) default NULL,
  `Value_error` varchar(30) default NULL,
  `Comment` varchar(80) default NULL,
  `date` date default NULL,
  PRIMARY KEY  (`Name`,`Property`,`Reference`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `otherproperties`
--


/*!40000 ALTER TABLE `otherproperties` DISABLE KEYS */;
LOCK TABLES `otherproperties` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `otherproperties` ENABLE KEYS */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- MySQL dump 10.10
--
-- Host: localhost    Database: astronomy
-- ------------------------------------------------------
-- Server version	5.0.13-rc-standard

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `papers`
--

DROP TABLE IF EXISTS `papers`;
CREATE TABLE `papers` (
  `Name` char(30) NOT NULL,
  `Component` char(30) NOT NULL,
  `Reference` varchar(80) NOT NULL,
  `Comment` varchar(80) NOT NULL,
  PRIMARY KEY  (`Name`,`Component`,`Reference`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `papers`
--


/*!40000 ALTER TABLE `papers` DISABLE KEYS */;
LOCK TABLES `papers` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `papers` ENABLE KEYS */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- MySQL dump 10.10
--
-- Host: localhost    Database: astronomy
-- ------------------------------------------------------
-- Server version	5.0.13-rc-standard

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `photometry`
--

DROP TABLE IF EXISTS `photometry`;
CREATE TABLE `photometry` (
  `Name` char(30) NOT NULL,
  `Band` char(20) NOT NULL,
  `Reference` varchar(80) NOT NULL default '',
  `Date` date NOT NULL default '0000-00-00',
  `Magnitude` float default NULL,
  `Mag_error` float default NULL,
  `Flux` float default NULL,
  `Flux_error` float default NULL,
  `Comment` varchar(80) default NULL,
  `good` tinyint(1) default NULL,
  PRIMARY KEY  (`Name`,`Band`,`Reference`,`Date`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `photometry`
--


/*!40000 ALTER TABLE `photometry` DISABLE KEYS */;
LOCK TABLES `photometry` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `photometry` ENABLE KEYS */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- MySQL dump 10.10
--
-- Host: localhost    Database: astronomy
-- ------------------------------------------------------
-- Server version	5.0.13-rc-standard

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `sources`
--

DROP TABLE IF EXISTS `sources`;
CREATE TABLE `sources` (
  `Name` char(30) NOT NULL,
  `ObjectType` varchar(15) default NULL,
  `SpectralType` varchar(15) default NULL,
  `Distance` int(11) default NULL,
  `Av` char(10) default '',
  `Notes` text,
  `RA2000` char(15) default NULL,
  `Dec2000` char(15) default NULL,
  `pmRA2000` float default NULL,
  `pmDec2000` float default NULL,
  `Reference_RADec` varchar(80) default NULL,
  `Reference_pm` varchar(80) default NULL,
  `Reference_distance` varchar(80) default NULL,
  `Reference_Av` varchar(80) NOT NULL default '',
  `HillenbrandClass` char(3) default NULL,
  `Priority` varchar(30) NOT NULL default '',
  `MIRdone` varchar(20) NOT NULL default '',
  `Lickdone` varchar(20) NOT NULL default '',
  `survey` varchar(40) default NULL,
  `Resolved` varchar(30) NOT NULL default '',
  `Multiple` varchar(30) default NULL,
  `NIR_resolved` tinyint(4) default NULL,
  `MIR_resolved` tinyint(4) default NULL,
  PRIMARY KEY  (`Name`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `sources`
--


/*!40000 ALTER TABLE `sources` DISABLE KEYS */;
LOCK TABLES `sources` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `sources` ENABLE KEYS */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- MySQL dump 10.10
--
-- Host: localhost    Database: astronomy
-- ------------------------------------------------------
-- Server version	5.0.13-rc-standard

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `AlternateNames`
--

DROP TABLE IF EXISTS `AlternateNames`;
CREATE TABLE `AlternateNames` (
  `Name` char(30) NOT NULL default '',
  `AlternateName` char(30) NOT NULL default '',
  PRIMARY KEY  (`Name`,`AlternateName`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `AlternateNames`
--


/*!40000 ALTER TABLE `AlternateNames` DISABLE KEYS */;
LOCK TABLES `AlternateNames` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `AlternateNames` ENABLE KEYS */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- MySQL dump 10.10
--
-- Host: localhost    Database: astronomy
-- ------------------------------------------------------
-- Server version	5.0.13-rc-standard

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `zeropoints`
--

DROP TABLE IF EXISTS `zeropoints`;
CREATE TABLE `zeropoints` (
  `instrument` varchar(30) NOT NULL,
  `datetime` datetime NOT NULL default '2000-01-01 00:00:00',
  `band` varchar(20) NOT NULL default '',
  `name` char(30) NOT NULL default '',
  `zeropoint` float NOT NULL,
  `zeropoint_err` float NOT NULL,
  `comment` varchar(80) NOT NULL default '',
  PRIMARY KEY  (`instrument`,`datetime`,`band`,`name`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `zeropoints`
--


/*!40000 ALTER TABLE `zeropoints` DISABLE KEYS */;
LOCK TABLES `zeropoints` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `zeropoints` ENABLE KEYS */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- MySQL dump 10.10
--
-- Host: localhost    Database: astronomy
-- ------------------------------------------------------
-- Server version	5.0.13-rc-standard

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `companions`
--

DROP TABLE IF EXISTS `companions`;
CREATE TABLE `companions` (
  `Name` char(30) NOT NULL default '',
  `Secondary` char(30) NOT NULL,
  `Reference` varchar(80) NOT NULL,
  `separation` float NOT NULL,
  `PA` float NOT NULL,
  `comment` varchar(80) default '',
  `band` char(20) default '',
  `delta_mag` char(5) default '',
  `date` date default '1000-01-01',
  PRIMARY KEY  (`Name`,`Secondary`,`Reference`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `companions`
--


/*!40000 ALTER TABLE `companions` DISABLE KEYS */;
LOCK TABLES `companions` WRITE;
UNLOCK TABLES;
/*!40000 ALTER TABLE `companions` ENABLE KEYS */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

