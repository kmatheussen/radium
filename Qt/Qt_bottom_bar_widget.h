/********************************************************************************
** Form generated from reading UI file 'qt4_bottom_bar_widget.ui'
**
** Created by: Qt User Interface Compiler version 4.8.2
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_QT4_BOTTOM_BAR_WIDGET_H
#define UI_QT4_BOTTOM_BAR_WIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QSlider>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_Bottom_bar_widget
{
public:
    QGridLayout *gridLayout;
    QFrame *frame;
    QHBoxLayout *horizontalLayout_2;
    QWidget *widget_2;
    QHBoxLayout *horizontalLayout;
    QLabel *status_label;
    QSpacerItem *horizontalSpacer;
    QFrame *line_5;
    MyQSlider *system_volume_slider;
    QFrame *line_7;
    QLabel *octave_label;
    QToolButton *octave_down_button;
    QToolButton *octave_up_button;
    QFrame *line_6;
    MyQSlider *min_velocity_slider;
    MyQSlider *velocity_slider;
    QToolButton *drunk_velocity_onoff;
    QFrame *line_8;
    QLabel *num_undos_label;
    QToolButton *undo_button;
    QToolButton *redo_button;
    QFrame *line_10;
    QToolButton *midi_input_onoff;
    QToolButton *scrollplay_onoff;
    QFrame *line_11;
    QLabel *cpu_label;

    void setupUi(QWidget *Bottom_bar_widget)
    {
        if (Bottom_bar_widget->objectName().isEmpty())
            Bottom_bar_widget->setObjectName(QString::fromUtf8("Bottom_bar_widget"));
        Bottom_bar_widget->resize(1014, 72);
        QSizePolicy sizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(Bottom_bar_widget->sizePolicy().hasHeightForWidth());
        Bottom_bar_widget->setSizePolicy(sizePolicy);
        gridLayout = new QGridLayout(Bottom_bar_widget);
        gridLayout->setContentsMargins(0, 0, 0, 0);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        frame = new QFrame(Bottom_bar_widget);
        frame->setObjectName(QString::fromUtf8("frame"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Maximum);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(frame->sizePolicy().hasHeightForWidth());
        frame->setSizePolicy(sizePolicy1);
        QPalette palette;
        QBrush brush(QColor(0, 0, 0, 255));
        brush.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::WindowText, brush);
        QBrush brush1(QColor(200, 200, 200, 255));
        brush1.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::Light, brush1);
        QBrush brush2(QColor(90, 90, 90, 255));
        brush2.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::Dark, brush2);
        palette.setBrush(QPalette::Active, QPalette::Text, brush);
        palette.setBrush(QPalette::Active, QPalette::ButtonText, brush);
        palette.setBrush(QPalette::Inactive, QPalette::WindowText, brush);
        palette.setBrush(QPalette::Inactive, QPalette::Light, brush1);
        palette.setBrush(QPalette::Inactive, QPalette::Dark, brush2);
        palette.setBrush(QPalette::Inactive, QPalette::Text, brush);
        palette.setBrush(QPalette::Inactive, QPalette::ButtonText, brush);
        palette.setBrush(QPalette::Disabled, QPalette::WindowText, brush2);
        palette.setBrush(QPalette::Disabled, QPalette::Light, brush1);
        palette.setBrush(QPalette::Disabled, QPalette::Dark, brush2);
        palette.setBrush(QPalette::Disabled, QPalette::Text, brush2);
        palette.setBrush(QPalette::Disabled, QPalette::ButtonText, brush2);
        frame->setPalette(palette);
        frame->setFrameShape(QFrame::Panel);
        frame->setFrameShadow(QFrame::Sunken);
        frame->setLineWidth(1);
        frame->setMidLineWidth(0);
        horizontalLayout_2 = new QHBoxLayout(frame);
        horizontalLayout_2->setContentsMargins(3, 3, 3, 3);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        widget_2 = new QWidget(frame);
        widget_2->setObjectName(QString::fromUtf8("widget_2"));
        QSizePolicy sizePolicy2(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(widget_2->sizePolicy().hasHeightForWidth());
        widget_2->setSizePolicy(sizePolicy2);
        horizontalLayout = new QHBoxLayout(widget_2);
        horizontalLayout->setContentsMargins(0, 0, 0, 0);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        status_label = new QLabel(widget_2);
        status_label->setObjectName(QString::fromUtf8("status_label"));
        QSizePolicy sizePolicy3(QSizePolicy::Expanding, QSizePolicy::Minimum);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(status_label->sizePolicy().hasHeightForWidth());
        status_label->setSizePolicy(sizePolicy3);

        horizontalLayout->addWidget(status_label);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);


        horizontalLayout_2->addWidget(widget_2);

        line_5 = new QFrame(frame);
        line_5->setObjectName(QString::fromUtf8("line_5"));
        QPalette palette1;
        QBrush brush3(QColor(121, 121, 121, 255));
        brush3.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::WindowText, brush3);
        QBrush brush4(QColor(174, 174, 174, 255));
        brush4.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::Button, brush4);
        QBrush brush5(QColor(195, 195, 195, 255));
        brush5.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::Light, brush5);
        QBrush brush6(QColor(214, 214, 214, 255));
        brush6.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::Midlight, brush6);
        QBrush brush7(QColor(87, 87, 87, 255));
        brush7.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::Dark, brush7);
        QBrush brush8(QColor(116, 116, 116, 255));
        brush8.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::Mid, brush8);
        QBrush brush9(QColor(84, 84, 84, 255));
        brush9.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::Text, brush9);
        QBrush brush10(QColor(205, 205, 205, 255));
        brush10.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::BrightText, brush10);
        QBrush brush11(QColor(117, 117, 117, 255));
        brush11.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::ButtonText, brush11);
        QBrush brush12(QColor(226, 226, 226, 255));
        brush12.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::Base, brush12);
        palette1.setBrush(QPalette::Active, QPalette::Window, brush4);
        palette1.setBrush(QPalette::Active, QPalette::Shadow, brush9);
        QBrush brush13(QColor(213, 213, 213, 255));
        brush13.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::HighlightedText, brush13);
        palette1.setBrush(QPalette::Active, QPalette::AlternateBase, brush6);
        QBrush brush14(QColor(113, 113, 113, 255));
        brush14.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::NoRole, brush14);
        QBrush brush15(QColor(212, 212, 183, 255));
        brush15.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::ToolTipBase, brush15);
        QBrush brush16(QColor(92, 92, 92, 255));
        brush16.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::ToolTipText, brush16);
        palette1.setBrush(QPalette::Inactive, QPalette::WindowText, brush3);
        palette1.setBrush(QPalette::Inactive, QPalette::Button, brush4);
        palette1.setBrush(QPalette::Inactive, QPalette::Light, brush5);
        palette1.setBrush(QPalette::Inactive, QPalette::Midlight, brush6);
        palette1.setBrush(QPalette::Inactive, QPalette::Dark, brush7);
        palette1.setBrush(QPalette::Inactive, QPalette::Mid, brush8);
        palette1.setBrush(QPalette::Inactive, QPalette::Text, brush9);
        palette1.setBrush(QPalette::Inactive, QPalette::BrightText, brush10);
        palette1.setBrush(QPalette::Inactive, QPalette::ButtonText, brush11);
        palette1.setBrush(QPalette::Inactive, QPalette::Base, brush12);
        palette1.setBrush(QPalette::Inactive, QPalette::Window, brush4);
        palette1.setBrush(QPalette::Inactive, QPalette::Shadow, brush9);
        palette1.setBrush(QPalette::Inactive, QPalette::HighlightedText, brush13);
        palette1.setBrush(QPalette::Inactive, QPalette::AlternateBase, brush6);
        palette1.setBrush(QPalette::Inactive, QPalette::NoRole, brush14);
        palette1.setBrush(QPalette::Inactive, QPalette::ToolTipBase, brush15);
        palette1.setBrush(QPalette::Inactive, QPalette::ToolTipText, brush16);
        palette1.setBrush(QPalette::Disabled, QPalette::WindowText, brush7);
        palette1.setBrush(QPalette::Disabled, QPalette::Button, brush4);
        palette1.setBrush(QPalette::Disabled, QPalette::Light, brush5);
        palette1.setBrush(QPalette::Disabled, QPalette::Midlight, brush6);
        palette1.setBrush(QPalette::Disabled, QPalette::Dark, brush7);
        palette1.setBrush(QPalette::Disabled, QPalette::Mid, brush8);
        palette1.setBrush(QPalette::Disabled, QPalette::Text, brush7);
        palette1.setBrush(QPalette::Disabled, QPalette::BrightText, brush10);
        palette1.setBrush(QPalette::Disabled, QPalette::ButtonText, brush7);
        palette1.setBrush(QPalette::Disabled, QPalette::Base, brush4);
        palette1.setBrush(QPalette::Disabled, QPalette::Window, brush4);
        palette1.setBrush(QPalette::Disabled, QPalette::Shadow, brush9);
        palette1.setBrush(QPalette::Disabled, QPalette::HighlightedText, brush13);
        palette1.setBrush(QPalette::Disabled, QPalette::AlternateBase, brush4);
        palette1.setBrush(QPalette::Disabled, QPalette::NoRole, brush14);
        palette1.setBrush(QPalette::Disabled, QPalette::ToolTipBase, brush15);
        palette1.setBrush(QPalette::Disabled, QPalette::ToolTipText, brush16);
        line_5->setPalette(palette1);
        line_5->setFrameShadow(QFrame::Plain);
        line_5->setLineWidth(1);
        line_5->setMidLineWidth(0);
        line_5->setFrameShape(QFrame::VLine);

        horizontalLayout_2->addWidget(line_5);

        system_volume_slider = new MyQSlider(frame);
        system_volume_slider->setObjectName(QString::fromUtf8("system_volume_slider"));
        QSizePolicy sizePolicy4(QSizePolicy::MinimumExpanding, QSizePolicy::Minimum);
        sizePolicy4.setHorizontalStretch(0);
        sizePolicy4.setVerticalStretch(0);
        sizePolicy4.setHeightForWidth(system_volume_slider->sizePolicy().hasHeightForWidth());
        system_volume_slider->setSizePolicy(sizePolicy4);
        system_volume_slider->setMinimumSize(QSize(120, 0));
        system_volume_slider->setMaximum(10000);
        system_volume_slider->setOrientation(Qt::Horizontal);

        horizontalLayout_2->addWidget(system_volume_slider);

        line_7 = new QFrame(frame);
        line_7->setObjectName(QString::fromUtf8("line_7"));
        QPalette palette2;
        palette2.setBrush(QPalette::Active, QPalette::WindowText, brush3);
        palette2.setBrush(QPalette::Active, QPalette::Button, brush4);
        palette2.setBrush(QPalette::Active, QPalette::Light, brush5);
        palette2.setBrush(QPalette::Active, QPalette::Midlight, brush6);
        palette2.setBrush(QPalette::Active, QPalette::Dark, brush7);
        palette2.setBrush(QPalette::Active, QPalette::Mid, brush8);
        palette2.setBrush(QPalette::Active, QPalette::Text, brush9);
        palette2.setBrush(QPalette::Active, QPalette::BrightText, brush10);
        palette2.setBrush(QPalette::Active, QPalette::ButtonText, brush11);
        palette2.setBrush(QPalette::Active, QPalette::Base, brush12);
        palette2.setBrush(QPalette::Active, QPalette::Window, brush4);
        palette2.setBrush(QPalette::Active, QPalette::Shadow, brush9);
        palette2.setBrush(QPalette::Active, QPalette::HighlightedText, brush13);
        palette2.setBrush(QPalette::Active, QPalette::AlternateBase, brush6);
        palette2.setBrush(QPalette::Active, QPalette::NoRole, brush14);
        palette2.setBrush(QPalette::Active, QPalette::ToolTipBase, brush15);
        palette2.setBrush(QPalette::Active, QPalette::ToolTipText, brush16);
        palette2.setBrush(QPalette::Inactive, QPalette::WindowText, brush3);
        palette2.setBrush(QPalette::Inactive, QPalette::Button, brush4);
        palette2.setBrush(QPalette::Inactive, QPalette::Light, brush5);
        palette2.setBrush(QPalette::Inactive, QPalette::Midlight, brush6);
        palette2.setBrush(QPalette::Inactive, QPalette::Dark, brush7);
        palette2.setBrush(QPalette::Inactive, QPalette::Mid, brush8);
        palette2.setBrush(QPalette::Inactive, QPalette::Text, brush9);
        palette2.setBrush(QPalette::Inactive, QPalette::BrightText, brush10);
        palette2.setBrush(QPalette::Inactive, QPalette::ButtonText, brush11);
        palette2.setBrush(QPalette::Inactive, QPalette::Base, brush12);
        palette2.setBrush(QPalette::Inactive, QPalette::Window, brush4);
        palette2.setBrush(QPalette::Inactive, QPalette::Shadow, brush9);
        palette2.setBrush(QPalette::Inactive, QPalette::HighlightedText, brush13);
        palette2.setBrush(QPalette::Inactive, QPalette::AlternateBase, brush6);
        palette2.setBrush(QPalette::Inactive, QPalette::NoRole, brush14);
        palette2.setBrush(QPalette::Inactive, QPalette::ToolTipBase, brush15);
        palette2.setBrush(QPalette::Inactive, QPalette::ToolTipText, brush16);
        palette2.setBrush(QPalette::Disabled, QPalette::WindowText, brush7);
        palette2.setBrush(QPalette::Disabled, QPalette::Button, brush4);
        palette2.setBrush(QPalette::Disabled, QPalette::Light, brush5);
        palette2.setBrush(QPalette::Disabled, QPalette::Midlight, brush6);
        palette2.setBrush(QPalette::Disabled, QPalette::Dark, brush7);
        palette2.setBrush(QPalette::Disabled, QPalette::Mid, brush8);
        palette2.setBrush(QPalette::Disabled, QPalette::Text, brush7);
        palette2.setBrush(QPalette::Disabled, QPalette::BrightText, brush10);
        palette2.setBrush(QPalette::Disabled, QPalette::ButtonText, brush7);
        palette2.setBrush(QPalette::Disabled, QPalette::Base, brush4);
        palette2.setBrush(QPalette::Disabled, QPalette::Window, brush4);
        palette2.setBrush(QPalette::Disabled, QPalette::Shadow, brush9);
        palette2.setBrush(QPalette::Disabled, QPalette::HighlightedText, brush13);
        palette2.setBrush(QPalette::Disabled, QPalette::AlternateBase, brush4);
        palette2.setBrush(QPalette::Disabled, QPalette::NoRole, brush14);
        palette2.setBrush(QPalette::Disabled, QPalette::ToolTipBase, brush15);
        palette2.setBrush(QPalette::Disabled, QPalette::ToolTipText, brush16);
        line_7->setPalette(palette2);
        line_7->setFrameShadow(QFrame::Plain);
        line_7->setLineWidth(1);
        line_7->setMidLineWidth(0);
        line_7->setFrameShape(QFrame::VLine);

        horizontalLayout_2->addWidget(line_7);

        octave_label = new QLabel(frame);
        octave_label->setObjectName(QString::fromUtf8("octave_label"));
        QSizePolicy sizePolicy5(QSizePolicy::Preferred, QSizePolicy::Minimum);
        sizePolicy5.setHorizontalStretch(0);
        sizePolicy5.setVerticalStretch(0);
        sizePolicy5.setHeightForWidth(octave_label->sizePolicy().hasHeightForWidth());
        octave_label->setSizePolicy(sizePolicy5);

        horizontalLayout_2->addWidget(octave_label);

        octave_down_button = new QToolButton(frame);
        octave_down_button->setObjectName(QString::fromUtf8("octave_down_button"));
        QSizePolicy sizePolicy6(QSizePolicy::Fixed, QSizePolicy::Minimum);
        sizePolicy6.setHorizontalStretch(0);
        sizePolicy6.setVerticalStretch(0);
        sizePolicy6.setHeightForWidth(octave_down_button->sizePolicy().hasHeightForWidth());
        octave_down_button->setSizePolicy(sizePolicy6);
        octave_down_button->setMaximumSize(QSize(16777215, 20));

        horizontalLayout_2->addWidget(octave_down_button);

        octave_up_button = new QToolButton(frame);
        octave_up_button->setObjectName(QString::fromUtf8("octave_up_button"));
        sizePolicy6.setHeightForWidth(octave_up_button->sizePolicy().hasHeightForWidth());
        octave_up_button->setSizePolicy(sizePolicy6);
        octave_up_button->setMaximumSize(QSize(16777215, 20));

        horizontalLayout_2->addWidget(octave_up_button);

        line_6 = new QFrame(frame);
        line_6->setObjectName(QString::fromUtf8("line_6"));
        QPalette palette3;
        palette3.setBrush(QPalette::Active, QPalette::WindowText, brush3);
        palette3.setBrush(QPalette::Active, QPalette::Button, brush4);
        palette3.setBrush(QPalette::Active, QPalette::Light, brush5);
        palette3.setBrush(QPalette::Active, QPalette::Midlight, brush6);
        palette3.setBrush(QPalette::Active, QPalette::Dark, brush7);
        palette3.setBrush(QPalette::Active, QPalette::Mid, brush8);
        palette3.setBrush(QPalette::Active, QPalette::Text, brush9);
        palette3.setBrush(QPalette::Active, QPalette::BrightText, brush10);
        palette3.setBrush(QPalette::Active, QPalette::ButtonText, brush11);
        palette3.setBrush(QPalette::Active, QPalette::Base, brush12);
        palette3.setBrush(QPalette::Active, QPalette::Window, brush4);
        palette3.setBrush(QPalette::Active, QPalette::Shadow, brush9);
        palette3.setBrush(QPalette::Active, QPalette::HighlightedText, brush13);
        palette3.setBrush(QPalette::Active, QPalette::AlternateBase, brush6);
        palette3.setBrush(QPalette::Active, QPalette::NoRole, brush14);
        palette3.setBrush(QPalette::Active, QPalette::ToolTipBase, brush15);
        palette3.setBrush(QPalette::Active, QPalette::ToolTipText, brush16);
        palette3.setBrush(QPalette::Inactive, QPalette::WindowText, brush3);
        palette3.setBrush(QPalette::Inactive, QPalette::Button, brush4);
        palette3.setBrush(QPalette::Inactive, QPalette::Light, brush5);
        palette3.setBrush(QPalette::Inactive, QPalette::Midlight, brush6);
        palette3.setBrush(QPalette::Inactive, QPalette::Dark, brush7);
        palette3.setBrush(QPalette::Inactive, QPalette::Mid, brush8);
        palette3.setBrush(QPalette::Inactive, QPalette::Text, brush9);
        palette3.setBrush(QPalette::Inactive, QPalette::BrightText, brush10);
        palette3.setBrush(QPalette::Inactive, QPalette::ButtonText, brush11);
        palette3.setBrush(QPalette::Inactive, QPalette::Base, brush12);
        palette3.setBrush(QPalette::Inactive, QPalette::Window, brush4);
        palette3.setBrush(QPalette::Inactive, QPalette::Shadow, brush9);
        palette3.setBrush(QPalette::Inactive, QPalette::HighlightedText, brush13);
        palette3.setBrush(QPalette::Inactive, QPalette::AlternateBase, brush6);
        palette3.setBrush(QPalette::Inactive, QPalette::NoRole, brush14);
        palette3.setBrush(QPalette::Inactive, QPalette::ToolTipBase, brush15);
        palette3.setBrush(QPalette::Inactive, QPalette::ToolTipText, brush16);
        palette3.setBrush(QPalette::Disabled, QPalette::WindowText, brush7);
        palette3.setBrush(QPalette::Disabled, QPalette::Button, brush4);
        palette3.setBrush(QPalette::Disabled, QPalette::Light, brush5);
        palette3.setBrush(QPalette::Disabled, QPalette::Midlight, brush6);
        palette3.setBrush(QPalette::Disabled, QPalette::Dark, brush7);
        palette3.setBrush(QPalette::Disabled, QPalette::Mid, brush8);
        palette3.setBrush(QPalette::Disabled, QPalette::Text, brush7);
        palette3.setBrush(QPalette::Disabled, QPalette::BrightText, brush10);
        palette3.setBrush(QPalette::Disabled, QPalette::ButtonText, brush7);
        palette3.setBrush(QPalette::Disabled, QPalette::Base, brush4);
        palette3.setBrush(QPalette::Disabled, QPalette::Window, brush4);
        palette3.setBrush(QPalette::Disabled, QPalette::Shadow, brush9);
        palette3.setBrush(QPalette::Disabled, QPalette::HighlightedText, brush13);
        palette3.setBrush(QPalette::Disabled, QPalette::AlternateBase, brush4);
        palette3.setBrush(QPalette::Disabled, QPalette::NoRole, brush14);
        palette3.setBrush(QPalette::Disabled, QPalette::ToolTipBase, brush15);
        palette3.setBrush(QPalette::Disabled, QPalette::ToolTipText, brush16);
        line_6->setPalette(palette3);
        line_6->setFrameShadow(QFrame::Plain);
        line_6->setLineWidth(1);
        line_6->setMidLineWidth(0);
        line_6->setFrameShape(QFrame::VLine);

        horizontalLayout_2->addWidget(line_6);

        min_velocity_slider = new MyQSlider(frame);
        min_velocity_slider->setObjectName(QString::fromUtf8("min_velocity_slider"));
        sizePolicy.setHeightForWidth(min_velocity_slider->sizePolicy().hasHeightForWidth());
        min_velocity_slider->setSizePolicy(sizePolicy);
        min_velocity_slider->setMaximum(10000);
        min_velocity_slider->setValue(4000);
        min_velocity_slider->setOrientation(Qt::Horizontal);

        horizontalLayout_2->addWidget(min_velocity_slider);

        velocity_slider = new MyQSlider(frame);
        velocity_slider->setObjectName(QString::fromUtf8("velocity_slider"));
        sizePolicy.setHeightForWidth(velocity_slider->sizePolicy().hasHeightForWidth());
        velocity_slider->setSizePolicy(sizePolicy);
        velocity_slider->setMaximum(10000);
        velocity_slider->setOrientation(Qt::Horizontal);

        horizontalLayout_2->addWidget(velocity_slider);

        drunk_velocity_onoff = new QToolButton(frame);
        drunk_velocity_onoff->setObjectName(QString::fromUtf8("drunk_velocity_onoff"));
        sizePolicy6.setHeightForWidth(drunk_velocity_onoff->sizePolicy().hasHeightForWidth());
        drunk_velocity_onoff->setSizePolicy(sizePolicy6);
        drunk_velocity_onoff->setMaximumSize(QSize(16777215, 20));
        drunk_velocity_onoff->setCheckable(true);
        drunk_velocity_onoff->setChecked(true);

        horizontalLayout_2->addWidget(drunk_velocity_onoff);

        line_8 = new QFrame(frame);
        line_8->setObjectName(QString::fromUtf8("line_8"));
        QPalette palette4;
        palette4.setBrush(QPalette::Active, QPalette::WindowText, brush3);
        palette4.setBrush(QPalette::Active, QPalette::Button, brush4);
        palette4.setBrush(QPalette::Active, QPalette::Light, brush5);
        palette4.setBrush(QPalette::Active, QPalette::Midlight, brush6);
        palette4.setBrush(QPalette::Active, QPalette::Dark, brush7);
        palette4.setBrush(QPalette::Active, QPalette::Mid, brush8);
        palette4.setBrush(QPalette::Active, QPalette::Text, brush9);
        palette4.setBrush(QPalette::Active, QPalette::BrightText, brush10);
        palette4.setBrush(QPalette::Active, QPalette::ButtonText, brush11);
        palette4.setBrush(QPalette::Active, QPalette::Base, brush12);
        palette4.setBrush(QPalette::Active, QPalette::Window, brush4);
        palette4.setBrush(QPalette::Active, QPalette::Shadow, brush9);
        palette4.setBrush(QPalette::Active, QPalette::HighlightedText, brush13);
        palette4.setBrush(QPalette::Active, QPalette::AlternateBase, brush6);
        palette4.setBrush(QPalette::Active, QPalette::NoRole, brush14);
        palette4.setBrush(QPalette::Active, QPalette::ToolTipBase, brush15);
        palette4.setBrush(QPalette::Active, QPalette::ToolTipText, brush16);
        palette4.setBrush(QPalette::Inactive, QPalette::WindowText, brush3);
        palette4.setBrush(QPalette::Inactive, QPalette::Button, brush4);
        palette4.setBrush(QPalette::Inactive, QPalette::Light, brush5);
        palette4.setBrush(QPalette::Inactive, QPalette::Midlight, brush6);
        palette4.setBrush(QPalette::Inactive, QPalette::Dark, brush7);
        palette4.setBrush(QPalette::Inactive, QPalette::Mid, brush8);
        palette4.setBrush(QPalette::Inactive, QPalette::Text, brush9);
        palette4.setBrush(QPalette::Inactive, QPalette::BrightText, brush10);
        palette4.setBrush(QPalette::Inactive, QPalette::ButtonText, brush11);
        palette4.setBrush(QPalette::Inactive, QPalette::Base, brush12);
        palette4.setBrush(QPalette::Inactive, QPalette::Window, brush4);
        palette4.setBrush(QPalette::Inactive, QPalette::Shadow, brush9);
        palette4.setBrush(QPalette::Inactive, QPalette::HighlightedText, brush13);
        palette4.setBrush(QPalette::Inactive, QPalette::AlternateBase, brush6);
        palette4.setBrush(QPalette::Inactive, QPalette::NoRole, brush14);
        palette4.setBrush(QPalette::Inactive, QPalette::ToolTipBase, brush15);
        palette4.setBrush(QPalette::Inactive, QPalette::ToolTipText, brush16);
        palette4.setBrush(QPalette::Disabled, QPalette::WindowText, brush7);
        palette4.setBrush(QPalette::Disabled, QPalette::Button, brush4);
        palette4.setBrush(QPalette::Disabled, QPalette::Light, brush5);
        palette4.setBrush(QPalette::Disabled, QPalette::Midlight, brush6);
        palette4.setBrush(QPalette::Disabled, QPalette::Dark, brush7);
        palette4.setBrush(QPalette::Disabled, QPalette::Mid, brush8);
        palette4.setBrush(QPalette::Disabled, QPalette::Text, brush7);
        palette4.setBrush(QPalette::Disabled, QPalette::BrightText, brush10);
        palette4.setBrush(QPalette::Disabled, QPalette::ButtonText, brush7);
        palette4.setBrush(QPalette::Disabled, QPalette::Base, brush4);
        palette4.setBrush(QPalette::Disabled, QPalette::Window, brush4);
        palette4.setBrush(QPalette::Disabled, QPalette::Shadow, brush9);
        palette4.setBrush(QPalette::Disabled, QPalette::HighlightedText, brush13);
        palette4.setBrush(QPalette::Disabled, QPalette::AlternateBase, brush4);
        palette4.setBrush(QPalette::Disabled, QPalette::NoRole, brush14);
        palette4.setBrush(QPalette::Disabled, QPalette::ToolTipBase, brush15);
        palette4.setBrush(QPalette::Disabled, QPalette::ToolTipText, brush16);
        line_8->setPalette(palette4);
        line_8->setFrameShadow(QFrame::Plain);
        line_8->setLineWidth(1);
        line_8->setMidLineWidth(0);
        line_8->setFrameShape(QFrame::VLine);

        horizontalLayout_2->addWidget(line_8);

        num_undos_label = new QLabel(frame);
        num_undos_label->setObjectName(QString::fromUtf8("num_undos_label"));

        horizontalLayout_2->addWidget(num_undos_label);

        undo_button = new QToolButton(frame);
        undo_button->setObjectName(QString::fromUtf8("undo_button"));

        horizontalLayout_2->addWidget(undo_button);

        redo_button = new QToolButton(frame);
        redo_button->setObjectName(QString::fromUtf8("redo_button"));

        horizontalLayout_2->addWidget(redo_button);

        line_10 = new QFrame(frame);
        line_10->setObjectName(QString::fromUtf8("line_10"));
        QPalette palette5;
        palette5.setBrush(QPalette::Active, QPalette::WindowText, brush3);
        palette5.setBrush(QPalette::Active, QPalette::Button, brush4);
        palette5.setBrush(QPalette::Active, QPalette::Light, brush5);
        palette5.setBrush(QPalette::Active, QPalette::Midlight, brush6);
        palette5.setBrush(QPalette::Active, QPalette::Dark, brush7);
        palette5.setBrush(QPalette::Active, QPalette::Mid, brush8);
        palette5.setBrush(QPalette::Active, QPalette::Text, brush9);
        palette5.setBrush(QPalette::Active, QPalette::BrightText, brush10);
        palette5.setBrush(QPalette::Active, QPalette::ButtonText, brush11);
        palette5.setBrush(QPalette::Active, QPalette::Base, brush12);
        palette5.setBrush(QPalette::Active, QPalette::Window, brush4);
        palette5.setBrush(QPalette::Active, QPalette::Shadow, brush9);
        palette5.setBrush(QPalette::Active, QPalette::HighlightedText, brush13);
        palette5.setBrush(QPalette::Active, QPalette::AlternateBase, brush6);
        palette5.setBrush(QPalette::Active, QPalette::NoRole, brush14);
        palette5.setBrush(QPalette::Active, QPalette::ToolTipBase, brush15);
        palette5.setBrush(QPalette::Active, QPalette::ToolTipText, brush16);
        palette5.setBrush(QPalette::Inactive, QPalette::WindowText, brush3);
        palette5.setBrush(QPalette::Inactive, QPalette::Button, brush4);
        palette5.setBrush(QPalette::Inactive, QPalette::Light, brush5);
        palette5.setBrush(QPalette::Inactive, QPalette::Midlight, brush6);
        palette5.setBrush(QPalette::Inactive, QPalette::Dark, brush7);
        palette5.setBrush(QPalette::Inactive, QPalette::Mid, brush8);
        palette5.setBrush(QPalette::Inactive, QPalette::Text, brush9);
        palette5.setBrush(QPalette::Inactive, QPalette::BrightText, brush10);
        palette5.setBrush(QPalette::Inactive, QPalette::ButtonText, brush11);
        palette5.setBrush(QPalette::Inactive, QPalette::Base, brush12);
        palette5.setBrush(QPalette::Inactive, QPalette::Window, brush4);
        palette5.setBrush(QPalette::Inactive, QPalette::Shadow, brush9);
        palette5.setBrush(QPalette::Inactive, QPalette::HighlightedText, brush13);
        palette5.setBrush(QPalette::Inactive, QPalette::AlternateBase, brush6);
        palette5.setBrush(QPalette::Inactive, QPalette::NoRole, brush14);
        palette5.setBrush(QPalette::Inactive, QPalette::ToolTipBase, brush15);
        palette5.setBrush(QPalette::Inactive, QPalette::ToolTipText, brush16);
        palette5.setBrush(QPalette::Disabled, QPalette::WindowText, brush7);
        palette5.setBrush(QPalette::Disabled, QPalette::Button, brush4);
        palette5.setBrush(QPalette::Disabled, QPalette::Light, brush5);
        palette5.setBrush(QPalette::Disabled, QPalette::Midlight, brush6);
        palette5.setBrush(QPalette::Disabled, QPalette::Dark, brush7);
        palette5.setBrush(QPalette::Disabled, QPalette::Mid, brush8);
        palette5.setBrush(QPalette::Disabled, QPalette::Text, brush7);
        palette5.setBrush(QPalette::Disabled, QPalette::BrightText, brush10);
        palette5.setBrush(QPalette::Disabled, QPalette::ButtonText, brush7);
        palette5.setBrush(QPalette::Disabled, QPalette::Base, brush4);
        palette5.setBrush(QPalette::Disabled, QPalette::Window, brush4);
        palette5.setBrush(QPalette::Disabled, QPalette::Shadow, brush9);
        palette5.setBrush(QPalette::Disabled, QPalette::HighlightedText, brush13);
        palette5.setBrush(QPalette::Disabled, QPalette::AlternateBase, brush4);
        palette5.setBrush(QPalette::Disabled, QPalette::NoRole, brush14);
        palette5.setBrush(QPalette::Disabled, QPalette::ToolTipBase, brush15);
        palette5.setBrush(QPalette::Disabled, QPalette::ToolTipText, brush16);
        line_10->setPalette(palette5);
        line_10->setFrameShadow(QFrame::Plain);
        line_10->setLineWidth(1);
        line_10->setMidLineWidth(0);
        line_10->setFrameShape(QFrame::VLine);

        horizontalLayout_2->addWidget(line_10);

        midi_input_onoff = new QToolButton(frame);
        midi_input_onoff->setObjectName(QString::fromUtf8("midi_input_onoff"));
        sizePolicy6.setHeightForWidth(midi_input_onoff->sizePolicy().hasHeightForWidth());
        midi_input_onoff->setSizePolicy(sizePolicy6);
        midi_input_onoff->setMaximumSize(QSize(16777215, 20));
        midi_input_onoff->setCheckable(true);

        horizontalLayout_2->addWidget(midi_input_onoff);

        scrollplay_onoff = new QToolButton(frame);
        scrollplay_onoff->setObjectName(QString::fromUtf8("scrollplay_onoff"));
        QSizePolicy sizePolicy7(QSizePolicy::Maximum, QSizePolicy::Minimum);
        sizePolicy7.setHorizontalStretch(0);
        sizePolicy7.setVerticalStretch(0);
        sizePolicy7.setHeightForWidth(scrollplay_onoff->sizePolicy().hasHeightForWidth());
        scrollplay_onoff->setSizePolicy(sizePolicy7);
        scrollplay_onoff->setMaximumSize(QSize(16777215, 20));
        scrollplay_onoff->setCheckable(true);
        scrollplay_onoff->setChecked(true);

        horizontalLayout_2->addWidget(scrollplay_onoff);

        line_11 = new QFrame(frame);
        line_11->setObjectName(QString::fromUtf8("line_11"));
        QPalette palette6;
        palette6.setBrush(QPalette::Active, QPalette::WindowText, brush3);
        palette6.setBrush(QPalette::Active, QPalette::Button, brush4);
        palette6.setBrush(QPalette::Active, QPalette::Light, brush5);
        palette6.setBrush(QPalette::Active, QPalette::Midlight, brush6);
        palette6.setBrush(QPalette::Active, QPalette::Dark, brush7);
        palette6.setBrush(QPalette::Active, QPalette::Mid, brush8);
        palette6.setBrush(QPalette::Active, QPalette::Text, brush9);
        palette6.setBrush(QPalette::Active, QPalette::BrightText, brush10);
        palette6.setBrush(QPalette::Active, QPalette::ButtonText, brush11);
        palette6.setBrush(QPalette::Active, QPalette::Base, brush12);
        palette6.setBrush(QPalette::Active, QPalette::Window, brush4);
        palette6.setBrush(QPalette::Active, QPalette::Shadow, brush9);
        palette6.setBrush(QPalette::Active, QPalette::HighlightedText, brush13);
        palette6.setBrush(QPalette::Active, QPalette::AlternateBase, brush6);
        palette6.setBrush(QPalette::Active, QPalette::NoRole, brush14);
        palette6.setBrush(QPalette::Active, QPalette::ToolTipBase, brush15);
        palette6.setBrush(QPalette::Active, QPalette::ToolTipText, brush16);
        palette6.setBrush(QPalette::Inactive, QPalette::WindowText, brush3);
        palette6.setBrush(QPalette::Inactive, QPalette::Button, brush4);
        palette6.setBrush(QPalette::Inactive, QPalette::Light, brush5);
        palette6.setBrush(QPalette::Inactive, QPalette::Midlight, brush6);
        palette6.setBrush(QPalette::Inactive, QPalette::Dark, brush7);
        palette6.setBrush(QPalette::Inactive, QPalette::Mid, brush8);
        palette6.setBrush(QPalette::Inactive, QPalette::Text, brush9);
        palette6.setBrush(QPalette::Inactive, QPalette::BrightText, brush10);
        palette6.setBrush(QPalette::Inactive, QPalette::ButtonText, brush11);
        palette6.setBrush(QPalette::Inactive, QPalette::Base, brush12);
        palette6.setBrush(QPalette::Inactive, QPalette::Window, brush4);
        palette6.setBrush(QPalette::Inactive, QPalette::Shadow, brush9);
        palette6.setBrush(QPalette::Inactive, QPalette::HighlightedText, brush13);
        palette6.setBrush(QPalette::Inactive, QPalette::AlternateBase, brush6);
        palette6.setBrush(QPalette::Inactive, QPalette::NoRole, brush14);
        palette6.setBrush(QPalette::Inactive, QPalette::ToolTipBase, brush15);
        palette6.setBrush(QPalette::Inactive, QPalette::ToolTipText, brush16);
        palette6.setBrush(QPalette::Disabled, QPalette::WindowText, brush7);
        palette6.setBrush(QPalette::Disabled, QPalette::Button, brush4);
        palette6.setBrush(QPalette::Disabled, QPalette::Light, brush5);
        palette6.setBrush(QPalette::Disabled, QPalette::Midlight, brush6);
        palette6.setBrush(QPalette::Disabled, QPalette::Dark, brush7);
        palette6.setBrush(QPalette::Disabled, QPalette::Mid, brush8);
        palette6.setBrush(QPalette::Disabled, QPalette::Text, brush7);
        palette6.setBrush(QPalette::Disabled, QPalette::BrightText, brush10);
        palette6.setBrush(QPalette::Disabled, QPalette::ButtonText, brush7);
        palette6.setBrush(QPalette::Disabled, QPalette::Base, brush4);
        palette6.setBrush(QPalette::Disabled, QPalette::Window, brush4);
        palette6.setBrush(QPalette::Disabled, QPalette::Shadow, brush9);
        palette6.setBrush(QPalette::Disabled, QPalette::HighlightedText, brush13);
        palette6.setBrush(QPalette::Disabled, QPalette::AlternateBase, brush4);
        palette6.setBrush(QPalette::Disabled, QPalette::NoRole, brush14);
        palette6.setBrush(QPalette::Disabled, QPalette::ToolTipBase, brush15);
        palette6.setBrush(QPalette::Disabled, QPalette::ToolTipText, brush16);
        line_11->setPalette(palette6);
        line_11->setFrameShadow(QFrame::Plain);
        line_11->setLineWidth(1);
        line_11->setMidLineWidth(0);
        line_11->setFrameShape(QFrame::VLine);

        horizontalLayout_2->addWidget(line_11);

        cpu_label = new QLabel(frame);
        cpu_label->setObjectName(QString::fromUtf8("cpu_label"));
        QSizePolicy sizePolicy8(QSizePolicy::Fixed, QSizePolicy::Preferred);
        sizePolicy8.setHorizontalStretch(0);
        sizePolicy8.setVerticalStretch(0);
        sizePolicy8.setHeightForWidth(cpu_label->sizePolicy().hasHeightForWidth());
        cpu_label->setSizePolicy(sizePolicy8);
        cpu_label->setMinimumSize(QSize(50, 0));
        cpu_label->setMaximumSize(QSize(50, 16777215));
        cpu_label->setBaseSize(QSize(80, 0));

        horizontalLayout_2->addWidget(cpu_label);


        gridLayout->addWidget(frame, 0, 0, 1, 1);


        retranslateUi(Bottom_bar_widget);

        QMetaObject::connectSlotsByName(Bottom_bar_widget);
    } // setupUi

    void retranslateUi(QWidget *Bottom_bar_widget)
    {
        Bottom_bar_widget->setWindowTitle(QApplication::translate("Bottom_bar_widget", "Form", 0, QApplication::UnicodeUTF8));
        status_label->setText(QApplication::translate("Bottom_bar_widget", "TextLabel", 0, QApplication::UnicodeUTF8));
        octave_label->setText(QApplication::translate("Bottom_bar_widget", "Octave: 4", 0, QApplication::UnicodeUTF8));
        octave_down_button->setText(QApplication::translate("Bottom_bar_widget", "-", 0, QApplication::UnicodeUTF8));
        octave_up_button->setText(QApplication::translate("Bottom_bar_widget", "+", 0, QApplication::UnicodeUTF8));
        drunk_velocity_onoff->setText(QApplication::translate("Bottom_bar_widget", "Drunk velocity", 0, QApplication::UnicodeUTF8));
        num_undos_label->setText(QApplication::translate("Bottom_bar_widget", "0", 0, QApplication::UnicodeUTF8));
        undo_button->setText(QApplication::translate("Bottom_bar_widget", "Undo", 0, QApplication::UnicodeUTF8));
        redo_button->setText(QApplication::translate("Bottom_bar_widget", "Redo", 0, QApplication::UnicodeUTF8));
        midi_input_onoff->setText(QApplication::translate("Bottom_bar_widget", "Midi input", 0, QApplication::UnicodeUTF8));
        scrollplay_onoff->setText(QApplication::translate("Bottom_bar_widget", "Scrollplay", 0, QApplication::UnicodeUTF8));
        cpu_label->setText(QApplication::translate("Bottom_bar_widget", "CPU:   55.2%", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class Bottom_bar_widget: public Ui_Bottom_bar_widget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_QT4_BOTTOM_BAR_WIDGET_H
