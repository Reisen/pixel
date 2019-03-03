import React      from 'react';
import classnames from 'classnames';
import styles     from './ToolTip.module.css';


interface Props {
    text: string;
}


const classes = (props: Props) => classnames({
    [styles.ToolTip]: true
})


export default (props: Props) =>
    <div className="ToolTip">
        {props.text}
    </div>;
