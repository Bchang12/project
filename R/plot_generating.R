#' Plot a boxplot or barplot of variables for inner dataset "heart"
#'
#' @param data using an inner dataset "heart"
#' @param variable "
#'
#' @return
#' @export
#'
#'
#' @examples
plot_generating=function(data=heart, variable){


  if (is.character(data[[variable]])){
    p1=ggplot()+geom_bar(aes(x=.data[[variable]]),data=data)

    if(variable=="cp"){
      p1_1= p1+labs(title="Chest pain type",
                    subtitle="0:typical angina, 1:atypical angina, 2:non-anginal pain, 3:asymptomatic" )+theme_bw()
      print(p1_1)
    }

    else if(variable=="fbs"){
      p1_1= p1+labs(title="fasting blood sugar > 120 mg/dl",
                    subtitle="0 = False, 1 = True" )+theme_bw()
      print(p1_1)
    }

    else if(variable=="restecg"){
      p1_1= p1+labs(title="resting electrocardiographic results")+theme_bw()
      print(p1_1)
    }

    else if(variable=="exng"){
      p1_1= p1+labs(title="exercise induced angina",
                    subtitle="0 = No, 1 = Yes" )+theme_bw()
      print(p1_1)
    }

    else if(variable=="slp"){
      p1_1= p1+labs(title="slope" )+theme_bw()
      print(p1_1)
    }

    else if(variable=="caa"){
      p1_1= p1+labs(title="number of major vessels" )+theme_bw()
      print(p1_1)
    }

    else if(variable=="thall"){
      p1_1= p1+labs(title="Thal rate" )+theme_bw()
      print(p1_1)
    }

    else{
      p1_1=p1+labs(title=variable)+theme_bw()
      print(p1_1)
    }
  }

  else if(variable=="output"){
    p3=ggplot()+geom_bar(aes(x=as.character(.data[[variable]])),data=data)+
      ggplot2::labs(x="output",title="Binary result of heart attack chance",
                    subtitle="0: less chance of heart attack\n1: more chance of heart attack")+ggplot2::theme_bw()
    print(p3)

  }


  else{
    p2=ggplot()+geom_boxplot(aes(x=.data[[variable]]),data=data)

    if(variable=="trtbps"){
      p2_1=p2+labs(title="resting blood pressure (in mm Hg)",x="resting blood pressure (in mm Hg)")+theme_bw()
      print(p2_1)
    }

    else if(variable=="chol"){
      p2_1=p2+labs(title="cholestoral in mg/dl",x="cholestoral in mg/dl")+theme_bw()
      print(p2_1)
    }

    else if(variable=="thalachh"){
      p2_1=p2+labs(title="maximum heart rate",x="maximum heart rate")+theme_bw()
      print(p2_1)
    }

    else if(variable=="oldpeak"){
      p2_1=p2+labs(title="Previous peak(oldpeak)",x="Previous peak(oldpeak)")+theme_bw()
      print(p2_1)
    }

    else{
      p2_1=p2+ggplot2::labs(title=paste(variable))+ggplot2::theme_bw()
      print(p2_1)}

  }
}

