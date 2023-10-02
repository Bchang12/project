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
    p1=ggplot2::ggplot()+ggplot2::geom_bar(aes(x=.data[[variable]]),data=data)

    if(variable=="cp"){
      p1_1= p1+ggplot2::labs(title="Chest pain type",
                    subtitle="0:typical angina, 1:atypical angina, 2:non-anginal pain, 3:asymptomatic" )+ggplot2::theme_bw()
      print(p1_1)
    }

    else if(variable=="fbs"){
      p1_1= p1+ggplot2::labs(title="fasting blood sugar > 120 mg/dl",
                    subtitle="0 = False, 1 = True" )+ggplot2::theme_bw()
      print(p1_1)
    }

    else if(variable=="restecg"){
      p1_1= p1+ggplot2::labs(title="resting electrocardiographic results")+ggplot2::theme_bw()
      print(p1_1)
    }

    else if(variable=="exng"){
      p1_1= p1+ggplot2::labs(title="exercise induced angina",
                    subtitle="0 = No, 1 = Yes" )+ggplot2::theme_bw()
      print(p1_1)
    }

    else if(variable=="slp"){
      p1_1= p1+ggplot2::labs(title="slope" )+ggplot2::theme_bw()
      print(p1_1)
    }

    else if(variable=="caa"){
      p1_1= p1+ggplot2::labs(title="number of major vessels" )+ggplot2::theme_bw()
      print(p1_1)
    }

    else if(variable=="thall"){
      p1_1= p1+ggplot2::labs(title="Thal rate" )+ggplot2::theme_bw()
      print(p1_1)
    }

    else{
      p1_1=p1+ggplot2::labs(title=variable)+ggplot2::theme_bw()
      print(p1_1)
    }
  }

  else if(variable=="output"){
    p3=ggplot2::ggplot()+ggplot2::geom_bar(aes(x=as.character(.data[[variable]])),data=data)+
      ggplot2::labs(x="output",title="Binary result of heart attack chance",
                    subtitle="0: less chance of heart attack\n1: more chance of heart attack")+ggplot2::theme_bw()
    print(p3)

  }


  else{
    p2=ggplot2::ggplot()+ggplot2::geom_boxplot(aes(x=.data[[variable]]),data=data)

    if(variable=="trtbps"){
      p2_1=p2+ggplot2::labs(title="resting blood pressure (in mm Hg)",x="resting blood pressure (in mm Hg)")+ggplot2::theme_bw()
      print(p2_1)
    }

    else if(variable=="chol"){
      p2_1=p2+ggplot2::labs(title="cholestoral in mg/dl",x="cholestoral in mg/dl")+ggplot2::theme_bw()
      print(p2_1)
    }

    else if(variable=="thalachh"){
      p2_1=p2+ggplot2::labs(title="maximum heart rate",x="maximum heart rate")+ggplot2::theme_bw()
      print(p2_1)
    }

    else if(variable=="oldpeak"){
      p2_1=p2+ggplot2::labs(title="Previous peak(oldpeak)",x="Previous peak(oldpeak)")+ggplot2::theme_bw()
      print(p2_1)
    }

    else{
      p2_1=p2+ggplot2::labs(title=paste(variable))+ggplot2::theme_bw()
      print(p2_1)}

  }
}

